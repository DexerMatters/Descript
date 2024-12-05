{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Subtyping where

import           Control.Monad (unless, zipWithM, forM, (>=>))
import           Control.Monad.State (gets, MonadState(put, get))
import           Data.Maybe (fromJust, catMaybes)
import           Data.Sequence (lookup)
import           State (ValState, isolate, putTypes)
import qualified Tm as T
import           Utils (Constraint(Bot, Top), secondM)
import qualified Val as V
import           Prelude hiding (lookup)
import           Data.Bool (bool)
import           Control.Monad.Error.Class (MonadError(throwError))
import           Dbg (printM)
import           Data.Graph (Tree(Node))
import           Data.Tree (levels, drawForest)
import           Errors (VTError(..))
import           Data.Foldable (Foldable(toList))

eval :: T.Ty -> ValState V.Ty
eval = \case
  T.TyPrim p          -> pure $ V.TyPrim p
  T.TyVar i l         -> do
    ty <- gets $ lookup l . V.types
    case ty of
      Just ty -> deep ty
      Nothing -> pure $ V.TyVar i
  T.TyArrow tys ty    -> V.TyArrow <$> mapM eval tys <*> eval ty
  T.TyTuple tys       -> V.TyTuple <$> mapM eval tys
  T.TyRcd tys         -> V.TyRcd <$> mapM (secondM eval) tys
  T.TyLam b i body    -> do
    env <- get
    return $ V.TyLam b i $ V.Closure env body
  T.TyApp ty tys tys' -> do
    args <- mapM eval tys
    holedArgs <- mapM eval tys'
    printM $ "TyApp: " ++ show args ++ " " ++ show holedArgs
    deduced <- zipWithM deduce holedArgs args
    printM $ "Deduced: " ++ drawForest (fmap (fmap show) deduced)
    realArgs <- bool
      {- Deduction with known args -}
      (catMaybes . concat . levels . Node Nothing
       <$> zipWithM deduce holedArgs args)
      {- Deduction without known args (Self-Deduction) -}
      (pure args)
      (null holedArgs)
    types <- gets V.types
    printM $ "Real args: " ++ show (toList types <> realArgs)
    printM $ "Function: " ++ show ty
    -- Evaluate the type with the yielded arguments
    isolate $ putTypes realArgs >> eval ty
  T.TyCast ty ty'     -> do
    ty <- eval ty
    ty' <- eval ty'
    b <- ty <: ty'
    unless b $ throwError $ BadCast ty ty'
    return ty'
  T.TyBiCast ty ty'   -> do
    ty <- eval ty
    ty' <- eval ty'
    b <- (&&) <$> ty <: ty' <*> ty' <: ty
    unless b $ throwError $ BadCast ty ty'
    return ty
  T.TyMacro _ _       -> throwError $ Unimplemented "Macro types"

($$) :: V.Closure -> [V.Ty] -> ValState V.Ty
($$) (V.Closure env tm) t = isolate $ put env >> putTypes t >> eval tm

(<:) :: V.Ty -> V.Ty -> ValState Bool
-- | Prim types are convertible only if they are the same
V.TyPrim p <: V.TyPrim p' = pure $ p == p'
-- | t is convertible to t if it is a subset of t's constraints
t <: V.TyVar i = do
  constrs <- gets (fromJust . lookup i . V.constrs)
  fmap and
    $ forM constrs
    $ \case
      Top a -> eval a >>= (t <:)
      Bot a -> eval a >>= (t <:)
V.TyVar i <: t = do
  constrs <- gets (fromJust . lookup i . V.constrs)
  fmap and
    $ forM constrs
    $ \case
      Bot a -> eval a >>= (<: t)
      Top a -> eval a >>= (<: t)
-- | Function types are convertible if their arguments are 
--   contravariant and their return types are covariant
V.TyArrow tys ty <: V.TyArrow tys' ty' = fmap and
  $ (:) <$> (ty <: ty') <*> zipWithM (<:) tys' tys
-- | Tuple types are convertible if their elements are convertible
V.TyTuple tys <: V.TyTuple tys' = and <$> zipWithM (<:) tys tys'
-- | Record types are convertible if their fields named the same 
--   are convertible and the second record has no extra fields
V.TyRcd flds <: V.TyRcd flds' = do
  printM $ "Rcd <: Rcd: " ++ show flds ++ " <: " ++ show flds'
  -- flds' is actually a sublist of flds
  s <- sequence [ty <: ty' | (l, ty) <- flds, (l', ty') <- flds', l == l']
  printM $ "Rcd <: Rcd: " ++ show s
  return $ and s && length flds' == length s
V.TyLam base i cls <: ty = do
  let vars = V.TyVar <$> [base .. base + i - 1]
  ret <- cls $$ vars
  ret <: ty
ty <: V.TyLam base i cls = do
  let vars = V.TyVar <$> [base .. base + i - 1]
  ret <- cls $$ vars
  ty <: ret
_ <: _ = pure False

evalStrict :: T.Ty -> ValState V.Ty
evalStrict = eval
  >=> \case
    V.TyVar x -> throwError $ AmbiguousType (V.TyVar x)
    ty        -> pure ty

type DeductionTree = Tree (Maybe V.Ty)

-- | Deduce the type with the given type and evidence
deduce :: V.Ty -> V.Ty -> ValState DeductionTree
deduce = curry
  $ \case
    (V.TyVar _, V.TyVar _) -> return $ Node Nothing []
    (V.TyVar i, t) -> do
      constrs <- gets (fromJust . lookup i . V.constrs)
      b <- fmap and
        $ forM constrs
        $ \case
          Bot a -> eval a >>= (t <:)
          Top a -> eval a >>= (<: t)
      unless b $ throwError $ BadMatchedBorder t constrs
      deduced <- forM constrs
        $ \case
          Top a -> eval a >>= flip deduce t
          Bot a -> eval a >>= flip deduce t
      return $ Node (Just t) (reverse deduced)
    ( V.TyTuple tys
      , V.TyTuple tys') -> Node Nothing <$> zipWithM deduce tys tys'
    (V.TyRcd flds, V.TyRcd flds') -> Node Nothing
      <$> sequence
        [deduce ty ty' | (l, ty) <- flds, (l', ty') <- flds', l == l']
    (V.TyArrow tys ty, V.TyArrow tys' ty') -> do
      ret <- deduce ty ty'
      args <- zipWithM deduce tys' tys
      return $ Node Nothing $ ret:args
    (V.TyLam base i cls, ty) -> do
      let vars = V.TyVar <$> [base .. base + i - 1]
      ret <- cls $$ vars
      deduce ret ty
    (ty, V.TyLam base i cls) -> do
      let vars = V.TyVar <$> [base .. base + i - 1]
      ret <- cls $$ vars
      deduce ty ret
    (ty, ty') -> ty' <: ty
      >>= bool (throwError $ BadCast ty' ty) (return $ Node Nothing [])

deep :: V.Ty -> ValState V.Ty
deep = \case
  V.TyTuple tys -> V.TyTuple <$> mapM deep tys
  V.TyRcd flds -> V.TyRcd <$> mapM (secondM deep) flds
  V.TyArrow tys ty -> V.TyArrow <$> mapM deep tys <*> deep ty
  V.TyApp ty tys -> V.TyApp <$> deep ty <*> mapM deep tys
  V.TyLam base i cls -> do
    let vars = V.TyVar <$> [base .. base + i - 1]
    ret <- cls $$ vars
    deep ret
  V.TyVar i -> do
    types <- gets (lookup i . V.types)
    case types of
      Just ty -> deep ty
      Nothing -> pure $ V.TyVar i
  ty -> pure ty
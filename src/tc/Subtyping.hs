{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Subtyping where

import           Control.Monad
import           Control.Monad.State
import           Data.Maybe (fromJust, fromMaybe, catMaybes)
import           Data.Sequence hiding (reverse, null, length)
import           State (ValState, isolate, putTypes)
import qualified Tm as T
import           Utils
import qualified Val as V
import           Prelude hiding (lookup)
import           Data.Bool (bool)
import           Control.Monad.Error.Class (MonadError(throwError))
import           Dbg (printM)
import           Data.Graph (Tree(Node))
import           Data.Tree (levels)
import           Errors (VTError(..))
import           Data.Functor ((<&>))

eval :: T.Ty -> ValState V.Ty
eval = \case
  T.TyPrim p          -> pure $ V.TyPrim p
  T.TyVar i l         -> gets $ fromMaybe (V.TyVar i l) . lookup l . V.types
  T.TyArrow tys ty    -> V.TyArrow <$> mapM eval tys <*> eval ty
  T.TyTuple tys       -> V.TyTuple <$> mapM eval tys
  T.TyRcd tys         -> V.TyRcd <$> mapM (secondM eval) tys
  T.TyLam i body      -> do
    env <- get
    return $ V.TyLam i $ V.Closure env body
  T.TyApp ty tys tys' -> do
    args <- mapM eval tys
    holedArgs <- mapM eval tys'
    printM $ "TyApp: " ++ show args ++ " " ++ show holedArgs
    deduced <- catMaybes . concat . levels . Node Nothing
      <$> zipWithM deduce holedArgs args
    printM $ "Deduced: " ++ show deduced
    realArgs <- bool
      {- Deduction with known args -}
      (catMaybes . concat . levels . Node Nothing
       <$> zipWithM deduce holedArgs args)
      {- Deduction without known args (Self-Deduction) -}
      (pure args)
      (null holedArgs)
    types <- gets V.types
    printM $ "Real args: " ++ show types
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
t <: V.TyVar i _ = do
  constrs <- gets (fromJust . lookup i . V.constrs)
  fmap and
    $ forM constrs
    $ \case
      Top a -> eval a >>= (t <:)
      Bot a -> eval a >>= (t <:)
V.TyVar i _ <: t = do
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
-- V.TyLam i cls <: ty = do
--   let base = length $ V.types (V.env cls)
--   let vars = V.TyVar <$> [base .. base + i - 1]
--   ret <- cls $$ vars
--   ret <: ty
-- ty <: V.TyLam i cls = do
--   let base = length $ V.types (V.env cls)
--   let vars = V.TyVar <$> [base .. base + i - 1]
--   ret <- cls $$ vars
--   ty <: ret
_ <: _ = pure False

type DeductionTree = Tree (Maybe V.Ty)

-- | Deduce the type with the given type and evidence
deduce :: V.Ty -> V.Ty -> ValState DeductionTree
deduce = curry
  $ \case
    (V.TyVar i _, t) -> do
      constrs <- gets (fromJust . lookup i . V.constrs)
      printM $ "Deduce: " ++ show t ++ " " ++ show constrs
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
      return $ Node (Just t) deduced
    ( V.TyTuple tys
      , V.TyTuple tys') -> Node Nothing <$> zipWithM deduce tys tys'
    (V.TyRcd flds, V.TyRcd flds') -> Node Nothing
      <$> sequence
        [deduce ty ty' | (l, ty) <- flds, (l', ty') <- flds', l == l']
    (V.TyArrow tys ty, V.TyArrow tys' ty') -> do
      args <- zipWithM deduce tys' tys
      ret <- deduce ty ty'
      return $ Node Nothing $ args ++ [ret]
    (ty, ty') -> ty' <: ty
      >>= bool (throwError $ BadCast ty' ty) (return $ Node Nothing [])

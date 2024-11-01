{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{-# LANGUAGE TupleSections #-}

module TypeEval where

import qualified Val as V
import qualified Tm as T
import           State
import           Utils
import           Control.Monad.State (gets, modify, MonadState(get, put))
import           Control.Monad (foldM, zipWithM, zipWithM_)
import           Data.Bool (bool)
import           Control.Monad.Error.Class (MonadError(throwError))
import           GHC.Base (join)

eval :: T.FITy --> V.FITy
eval = \case
  p :| T.TyVar i        -> do
    env <- gets V.types
    return $ p :| env !! i
  p :| T.TyPrim prim    -> return $ p :| V.TyPrim prim
  p :| T.TyArrow tys ty -> do
    tys' <- mapM eval tys
    ty' <- eval ty
    return $ p :| V.TyArrow tys' ty'
  p :| T.TyTuple tys    -> do
    tys' <- mapM eval tys
    return $ p :| V.TyTuple tys'
  p :| T.TyRcd flds     -> do
    flds' <- mapM (\(l, t) -> (l, ) <$> eval t) flds
    return $ p :| V.TyRcd flds'
  p :| T.TyLam i cls    -> do
    env <- get
    return $ p :| V.TyLam i (V.Closure env cls)
  _ :| T.TyCast a b     -> join $ cast <$> eval a <*> eval b
  _ :| T.TyBiCast a b   -> join $ bicast <$> eval a <*> eval b
  _ :| T.TyReduce f as  -> do
    func <- eval f
    args <- mapM eval as
    case func of
      _ :| V.TyArrow args' ty -> do
        zipWithM_ cast args args'
        return ty
      _ :| V.TyLam n cls -> do
        tyArgs <- inferTypeArgs 

-- | Convert a type to another possible type
cast :: V.FITy -> V.FITy --> V.FITy
cast a b = a <: b >>= bool (throwError $ BadCast a b) (return b)

bicast :: V.FITy -> V.FITy --> V.FITy
bicast a b = (,) <$> a <: b <*> b <: a
  >>= \case
    (True, True)  -> return b
    (True, False) -> throwError $ BadCast b a
    (False, _)    -> throwError $ BadCast a b

-- | Compare two types. Check if rhs is a subtype of lhs,
--   which is to say lhs is more general than rhs.
(<:) :: V.FITy -> V.FITy --> Bool
(<:) = curry
  $ \case
    --  Top is a supertype of all types
    V.TyBot :<*>: _ -> return True
    -- Bot is a subtype of all types
    _ :<*>: V.TyTop -> return True
    -- Different primitive types do not differ in generality
    V.TyPrim p1 :<*>: V.TyPrim p2 -> return $ p1 == p2
    p :| V.TyVar i :*: p' :| V.TyVar j -> do
      (top, bot) <- evalBorder (p :| i)
      (top', bot') <- evalBorder (p' :| j)
      b <- top' <: top
      b' <- bot <: bot'
      return $ b && b'
    V.TyTuple tys :<*>: V.TyTuple tys' -> and <$> zipWithM (<:) tys tys'
    -- Record A is a subtype of record B only if
    -- - A's domain is a subset of B's
    -- - For each label l in A, A[l] <: B[l]
    V.TyRcd flds :<*>: V.TyRcd flds' -> do
      let f (l, a) (l', b) = (&&) (l == l') <$> (a <: b)
      includeByM f flds flds'
    -- A function type A -> B is a subtype of C -> D only if
    -- - C <: A (contravariant)
    -- - B <: D (covariant)
    V.TyArrow tys ty :<*>: V.TyArrow tys' ty' -> do
      tys'' <- zipWithM (<:) tys' tys -- Contravariant
      ty'' <- ty <: ty' -- Covariant
      return $ and (ty'':tys'')
    -- A polymorphic type can be compared when instantiated with variables
    V.TyLam i cls :<*>: V.TyLam i' cls' -> do
      ret <- apply cls (extendCtx cls i)
      ret' <- apply cls' (extendCtx cls' i')
      ret <: ret'
    V.TyLam i cls :<*: p' :| ty -> do
      ret <- apply cls (extendCtx cls i)
      ret <: FI p' ty
    p :| ty :*>: V.TyLam i cls -> do
      let base = length . V.types $ V.env cls
      let vars = V.TyVar <$> [base .. base + i - 1]
      ret <- apply cls vars
      FI p ty <: ret
    _ -> return False
  where
    -- | Introduce the type variables to the context
    extendCtx :: V.Closure -> Int -> [V.Ty]
    extendCtx (V.Closure env _) i = let base = length . V.types $ env
                                    in V.TyVar <$> [base .. base + i - 1]

-- | Reduce a closure by providing the arguments
apply :: V.Closure -> [V.Ty] --> V.FITy
apply (V.Closure env tm) args = do
  env0 <- get
  put $ env { V.types = reverse args ++ V.types env }
  res <- eval tm
  put env0
  return res

-- | Get the border of a type variable while 
--   evaluating and normalizing its constraints
evalBorder :: FI Int --> V.Border
evalBorder (p :| i) = do
  bdrs <- gets V.border
  res <- evalStateM norm (bdrs !! i)
  let updated = replace bdrs i res
  -- Memorize the updated border
  modify (\s -> s { V.border = updated })
  return (fromInterpreted res)
  where
    -- | Shrink the constraints to the smallest possible border
    norm c@(T.Constr _ tops bots) = do
      -- env0 <- get
      -- TODO: Switch to the environment of the constraint
      --       and evaluate the tops and bots, but environment
      --       should be evaluated (to be a TCtx)
      tops' <- mapM eval (FI p <$> tops)
      bots' <- mapM eval (FI p <$> bots)
      top <- foldM (botmost c) (p :| V.TyBot) tops'
      bot <- foldM (topmost c) (p :| V.TyTop) bots'
      return (top, bot)

    -- | Compute the topmost type of two types
    topmost c lhs rhs = (,) <$> lhs <: rhs <*> rhs <: lhs
      >>= \case
        (True, _)      -> return lhs
        (False, True)  -> return rhs
        (False, False) -> throwError $ BadConstraint $ p :| c

    -- | Compute the botmost type of two types
    botmost c lhs rhs = (,) <$> lhs <: rhs <*> rhs <: lhs
      >>= \case
        (_, True)      -> return lhs
        (True, False)  -> return rhs
        (False, False) -> throwError $ BadConstraint $ p :| c
evalBorder _ = error "impossible"

inferTypeArgs :: V.FITy -> V.FITy --> [V.Ty]
inferTypeArgs = curry $ \case
  -- {To be match} :<*>: {Input}
  p :| V.TyVar i :*>: ty -> do
    (top, bot) <- evalBorder (p :| i)
    ts <- inferTypeArgs top ty
    bs <- inferTypeArgs bot ty
    return $ ty : ts ++ bs
  V.TyTuple tys :<*>: V.TyTuple tys' -> 
    join <$> zipWithM inferTypeArgs tys tys'
  V.TyRcd flds :<*>: V.TyRcd flds' ->
    let sames = [(t, t') | (l, t) <- flds, (l', t') <- flds', l == l']
    in join <$> mapM (uncurry inferTypeArgs) sames
  V.TyArrow tys ty :<*>: V.TyArrow tys' ty' -> do
    tys'' <- zipWithM inferTypeArgs tys tys'
    ty'' <- inferTypeArgs ty ty'
    return $ ty'' : tys''
  V.TyLam _ _ :<*>: _ -> throwError $ NonDeducibleArgumentType ty
  _ :<*>: _ -> return []
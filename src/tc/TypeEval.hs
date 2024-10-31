{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module TypeEval where

import qualified Val as V
import qualified Tm as T
import           State
import           Utils
import           Control.Monad.State (gets, evalState, modify
                                    , MonadState(get, put))
import           Control.Monad (foldM, zipWithM)
import           Data.Bool (bool)
import           Control.Monad.Error.Class (MonadError(throwError))
import           Control.Arrow (Arrow(second))

eval :: T.FITy --> V.Ty
eval = undefined

(=>>) :: T.FITy -> T.FITy --> V.Ty
(=>>) = undefined

(<:) :: V.Ty -> V.Ty --> Tril
(<:) = curry
  $ \case
    V.TyBot :<>: _ -> return True'
    _ :<>: V.TyTop -> return True'
    V.TyPrim p1 :<>: V.TyPrim p2 -> return $ fromBool $ p1 == p2
    V.TyTuple tys :<>: V.TyTuple tys' -> allT
      <$> zipWithM (<:) (val <$> tys) (val <$> tys')
    V.TyRcd flds :<>: V.TyRcd flds' -> do
      let f (l, a) (l', b) = ((&&&)) (l `trEq` l') <$> (a <: b)
      let flds1 = second val <$> flds
      let flds2 = second val <$> flds'
      includeByM f flds1 flds2
    V.TyArrow tys ty :<>: V.TyArrow tys' ty' -> do
      tys'' <- zipWithM (<:) (val <$> tys) (val <$> tys')
      ty'' <- val ty <: val ty'
      return $ allT (ty'':tys'')
    V.TyLam i (V.Closure env tm) :<>: V.TyLam i' (V.Closure env' tm') -> do
      

apply :: V.Closure -> [V.Ty] --> V.Ty
apply (V.Closure env tm) args = do
  env0 <- get
  put $ env { V.types = reverse args ++ V.types env }
  res <- eval tm
  put env0
  return res

  -- Switch to the environment of the closure

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
      top <- foldM (botmost c) V.TyBot tops'
      bot <- foldM (topmost c) V.TyTop bots'
      return (p :| top, p :| bot)

    -- | Compute the topmost type of two types
    topmost c lhs rhs = lhs <: rhs
      >>= tril (return lhs) (return rhs) (throwError $ BadConstraint $ p :| c)

    -- | Compute the botmost type of two types
    botmost c lhs rhs = lhs <: rhs
      >>= tril (return rhs) (return lhs) (throwError $ BadConstraint $ p :| c)
evalBorder _ = error "impossible"

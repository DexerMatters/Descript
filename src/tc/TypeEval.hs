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
import           Control.Monad (foldM, zipWithM, zipWithM_, unless)
import           Data.Bool (bool)
import           Control.Monad.Error.Class (MonadError(throwError))
import           GHC.Base (join)
import           Dbg (traceInfo, printInfo, printM)

eval :: T.FITy --> V.FITy
eval = \case
  p :| T.TyVar i -> do
    env <- gets V.types
    return $ p :| env !! i
  p :| T.TyPrim prim -> return $ p :| V.TyPrim prim
  p :| T.TyArrow tys ty -> do
    tys' <- mapM eval tys
    ty' <- eval ty
    return $ p :| V.TyArrow tys' ty'
  p :| T.TyTuple tys -> do
    tys' <- mapM eval tys
    return $ p :| V.TyTuple tys'
  p :| T.TyRcd flds -> do
    flds' <- mapM (\(l, t) -> (l, ) <$> eval t) flds
    return $ p :| V.TyRcd flds'
  p :| T.TyLam i cls -> do
    env <- get
    return $ p :| V.TyLam i (V.Closure env cls)
  _ :| T.TyCast a b -> join $ cast <$> eval a <*> eval b
  _ :| T.TyBiCast a b -> join $ bicast <$> eval a <*> eval b
  _ :| T.TyReduce f as -> do
    func <- eval f
    args <- mapM eval as
    let aux = \case
          _ :| V.TyArrow args' ty -> do
            zipWithM_ cast args args'
            printM $ "ArgTypes: " ++ show [args, args']
            return ty
          _ :| V.TyLam i cls -> do
            argTypes <- folkEnv
              $ do
                ret <- reduce cls i
                case ret of
                  _ :| V.TyArrow args' _
                    -> join <$> zipWithM inferTypeArgs args' args
                  _ -> throwError $ NotAFunctionType func
            printM $ "ArgTypes: " ++ show argTypes
            aux =<< folkEnv (apply cls argTypes)
          _ -> throwError $ NotAFunctionType func
    aux func
  _ :| T.TyApp f as -> do
    tFunc <- eval f
    tArgs <- mapM eval as
    case tFunc of
      _ :| V.TyLam i cls
        | i == length tArgs -> apply cls (val <$> tArgs)
      _ -> throwError $ NotATypeFunctionType tFunc
  _ :| T.TyMacro _ ty -> do
    eval ty
  _ -> error "impossible"

        -- With 

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
-- *more specific* <: *more general*
(<:) = curry
  $ \case
    --  Top is a supertype of all types
    _ :<*>: V.TyTop -> return True
    -- Bot is a subtype of all types
    V.TyBot :<*>: _ -> return True
    -- Different primitive types do not differ in generality
    V.TyPrim p1 :<*>: V.TyPrim p2 -> return $ p1 == p2
    p :| V.TyVar i :*: p' :| V.TyVar j -> do
      (bot, top) <- evalBorder (p :| i) -- smaller
      (bot', top') <- evalBorder (p' :| j)
      b <- top <: top'
      b' <- bot' <: bot
      return $ b && b'
    p :| V.TyVar i :*: ty -> do
      (bot, top) <- evalBorder (p :| i)
      printM $ show (bot, top)
      b <- bot <: ty
      b' <- ty <: top
      return $ b && b'
    ty :*: p :| V.TyVar i -> do
      (bot, top) <- evalBorder (p :| i)
      b <- ty <: top
      b' <- bot <: ty
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
      ret <- folkEnv $ reduce cls i
      ret' <- folkEnv $ reduce cls' i'
      ret <: ret'
    V.TyLam i cls :<*: p' :| ty -> do
      ret <- reduce cls i
      ret <: FI p' ty
    p :| ty :*>: V.TyLam i cls -> do
      ret <- reduce cls i
      FI p ty <: ret
    -- Union and sum types
    V.TyUnion t1 t2 :<*: t -> (||) <$> t1 <: t <*> t2 <: t
    t :*>: V.TyUnion t1 t2 -> (||) <$> t <: t1 <*> t <: t2
    V.TySum t1 t2 :<*: t -> (&&) <$> t1 <: t <*> t2 <: t
    t :*>: V.TySum t1 t2 -> (&&) <$> t <: t1 <*> t <: t2
    _ -> return False

-- | Introduce the type variables to the context
extendCtx :: V.Closure -> Int -> [V.Ty]
extendCtx (V.Closure env _) i = let base = length . V.types $ env
                                in V.TyVar <$> [base .. base + i - 1]

-- | Reduce a closure by providing the arguments
apply :: V.Closure -> [V.Ty] --> V.FITy
apply (V.Closure env tm) args = do
  put $ env { V.types = args ++ V.types env }
  eval tm

-- | Reduce a closure without providing the arguments
reduce :: V.Closure -> Int --> V.FITy
reduce cls@(V.Closure env _) i = do
  let base = length . V.types $ env
  let vars = V.TyVar <$> [base .. base + i - 1]
  apply cls vars

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
    norm (T.Constr _ tops bots _) = do
      -- env0 <- get
      -- TODO: Switch to the environment of the constraint
      --       and evaluate the tops and bots, but environment
      --       should be evaluated (to be a TCtx)
      tops' <- mapM eval (FI p <$> tops)
      bots' <- mapM eval (FI p <$> bots)
      top <- if null tops'
             then return $ p :| V.TyTop
             else foldM botmost (p :| V.TyBot) tops'
      bot <- if null bots'
             then return $ p :| V.TyBot
             else foldM topmost (p :| V.TyTop) bots'
      return (bot, top)

    -- | Compute the more specific type of two types
    topmost lhs rhs = (,) <$> lhs <: rhs <*> rhs <: lhs
      >>= \case
        (True, _)      -> return lhs
        (False, True)  -> return rhs
        (False, False) -> return $ p :| V.TySum lhs rhs

    -- | Compute the more general type of two types
    botmost lhs rhs = (,) <$> lhs <: rhs <*> rhs <: lhs
      >>= \case
        (_, True)      -> return lhs
        (True, False)  -> return rhs
        (False, False) -> return $ p :| V.TyUnion lhs rhs
evalBorder _ = error "impossible"

inferTypeArgs :: V.FITy -> V.FITy --> [V.Ty]
inferTypeArgs = curry
  $ \case
    -- {To be match} :<*>: {Input}
    p :| V.TyVar i :*: ty -> do
      (bot, top) <- evalBorder (p :| i)
      b <- bot <: ty
      b' <- ty <: top
      printM $ "inferTypeArgs: " ++ show [ty, bot, top]
      unless (b && b') $ throwError $ BadParameterType ty (bot, top)
      ts <- inferTypeArgs top ty
      bs <- inferTypeArgs bot ty
      return $ val ty:ts ++ bs
    V.TyTuple tys
      :<*>: V.TyTuple tys' -> join <$> zipWithM inferTypeArgs tys tys'
    V.TyRcd flds :<*>: V.TyRcd flds'
      -> let sames = [(t, t') | (l, t) <- flds, (l', t') <- flds', l == l']
         in join <$> mapM (uncurry inferTypeArgs) sames
    V.TyArrow tys ty :<*>: V.TyArrow tys' ty' -> do
      tys'' <- join <$> zipWithM inferTypeArgs tys tys'
      ty'' <- inferTypeArgs ty ty'
      return $ ty'' ++ tys''
    V.TyLam _ _ :<*: ty -> throwError $ NonDeducibleArgumentType ty
    _ -> return []


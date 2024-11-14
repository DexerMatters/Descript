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
import           Dbg (printM)

eval :: T.FITy --> V.FITy
eval = \case
  p :| T.TyVar i -> do
    env <- gets V.types
    if i < length env
      then return $ p :| env !! i
      else return $ p :| V.TyVar i
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
  _ :| T.TyReduce tarArg f as -> do
    func <- eval f
    args <- mapM eval as
    tarArgs <- mapM eval tarArg
    printM $ "ArgTypes: " ++ show tarArgs
    let aux = \case
          _ :| V.TyArrow args' ty -> do
            zipWithM_ cast args args'
            return ty
          _ :| V.TyLam i cls -> do
            argTypes <- join <$> zipWithM inferTypeArgs tarArgs args
            unless (length argTypes == i)
              $ throwError
              $ DissatisfiedTypeParameterCount (FI (pos f) i)
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
(<:) = curry
  $ \case
    --  Top is a supertype of all types
    _ :<*>: V.TyTop -> return True
    -- Bot is a subtype of all types
    V.TyBot :<*>: _ -> return True
    -- Different primitive types do not differ in generality
    V.TyPrim p1 :<*>: V.TyPrim p2 -> return $ p1 == p2
    p :| V.TyVar i :*: p' :| V.TyVar j -> do
      (top, bot) <- evalBorder (p :| i)
      (top', bot') <- evalBorder (p' :| j)
      b <- top' <: top
      b' <- bot <: bot'
      return $ b && b'
    p :| V.TyVar i :*: ty -> do
      (top, bot) <- evalBorder (p :| i)
      printM $ show ty ++ "," ++ show top ++ "," ++ show bot
      b <- ty <: top
      b' <- bot <: ty
      return $ b && b'
    ty :*: p :| V.TyVar i -> do
      (top, bot) <- evalBorder (p :| i)
      b <- ty <: bot
      b' <- top <: ty
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
    V.TyUnion a b :<*: ty -> (||) <$> (a <: ty) <*> (b <: ty)
    ty :*>: V.TyUnion a b -> (&&) <$> (ty <: a) <*> (ty <: b)
    V.TySum a b :<*: ty -> (&&) <$> (a <: ty) <*> (b <: ty)
    ty :*>: V.TySum a b -> (||) <$> (ty <: a) <*> (ty <: b)
    _ -> return False

getLowerType :: V.FITy -> V.FITy --> V.FITy
getLowerType t1 t2 = case t1 :*: t2 of
  V.TyTop :<*>: _ -> return t2
  _ :<*>: V.TyTop -> return t1
  V.TyBot :<*>: _ -> return t1
  _ :<*>: V.TyBot -> return t2
  V.TyRcd flds :<*>: V.TyRcd flds' -> do
    newFlds <- sequence
      [(l, ) <$> getLowerType t t'
      | (l, t) <- flds
      , (l', t') <- flds'
      , l == l']
    return $ pos t1 :| V.TyRcd newFlds
  V.TyTuple tys :<*>: V.TyTuple tys'
    | length tys == length tys' -> FI (pos t1) . V.TyTuple
      <$> zipWithM getLowerType tys tys'
  V.TyArrow tys ty :<*>: V.TyArrow tys' ty'
    | length tys == length tys' -> do
      tys'' <- zipWithM getUpperType tys tys'
      ty'' <- getLowerType ty ty'
      return $ pos t1 :| V.TyArrow tys'' ty''
  other :*: another -> return $ pos t1 :| V.TyUnion other another
  _ -> error "impossible"

getUpperType :: V.FITy -> V.FITy --> V.FITy
getUpperType t1 t2 = case t1 :*: t2 of
  V.TyTop :<*>: _ -> return t1
  _ :<*>: V.TyTop -> return t2
  V.TyBot :<*>: _ -> return t2
  _ :<*>: V.TyBot -> return t1
  V.TyPrim p :<*>: V.TyPrim p'
    | p == p' -> return t1
  V.TyRcd flds :<*>: V.TyRcd flds' -> do
    newFlds <- sequence
      $ do
        (l, t) <- flds
        (l', t') <- flds'
        if l == l'
          then [(l, ) <$> getUpperType t t']
          else [pure (l, t), pure (l', t')]
    return $ pos t1 :| V.TyRcd newFlds
  V.TyTuple tys :<*>: V.TyTuple tys'
    | length tys == length tys' -> FI (pos t1) . V.TyTuple
      <$> zipWithM getUpperType tys tys'
  V.TyArrow tys ty :<*>: V.TyArrow tys' ty'
    | length tys == length tys' -> do
      tys'' <- zipWithM getLowerType tys tys'
      ty'' <- getUpperType ty ty'
      return $ pos t1 :| V.TyArrow tys'' ty''
  other :*: another -> return $ pos t1 :| V.TySum other another
  _ -> error "impossible"

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
    norm (T.Constr tops bots _) = do
      -- env0 <- get
      -- TODO: Switch to the environment of the constraint
      --       and evaluate the tops and bots, but environment
      --       should be evaluated (to be a TCtx)
      tops' <- mapM eval (FI p <$> tops)
      bots' <- mapM eval (FI p <$> bots)
      top <- foldM getLowerType (p :| V.TyTop) tops'
      bot <- foldM getUpperType (p :| V.TyBot) bots'
      return (top, bot)
evalBorder _ = error "impossible"

inferTypeArgs :: V.FITy -> V.FITy --> [V.Ty]
inferTypeArgs = curry
  $ \case
    -- {To be match} :<*>: {Input}
    p :| V.TyVar i :*: ty -> do
      (top, bot) <- evalBorder (p :| i)
      ts <- inferTypeArgs top ty
      bs <- inferTypeArgs bot ty
      b <- (&&) <$> (ty <: top) <*> (bot <: ty)
      printM $ show ty ++ ",bot: " ++ show bot ++ ",top: " ++ show top
      unless b $ throwError $ BadMatchedBorder ty top bot
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


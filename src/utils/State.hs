{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module State where

import           Control.Monad.Except (ExceptT, MonadError(throwError)
                                     , runExceptT)
import           Control.Monad.State (MonadState(get, put), State, modify, gets
                                    , runState, evalState, StateT(runStateT))
import           Data.Map (lookup, insert, updateAt, elems)
import           Raw as R
import           Tm as T
import           Utils
import           Val as V
import           Prelude hiding (lookup)

--------------------------------------------------------------------------------
-- States
--------------------------------------------------------------------------------

type PartialState e s a = ExceptT e (State s) a

type TCState a = PartialState TCError Ctx a

type TEState a = PartialState TEError TCtx a

type (->>) a b = a -> TCState b

type (-->) a b = a -> TEState b

--------------------------------------------------------------------------------
-- Context
--------------------------------------------------------------------------------

-- | Run a partial state with an initial context.
runPartialState :: s -> PartialState e s a -> (Either e a, s)
runPartialState s m = runState (runExceptT m) s

-- | Run a type-checking state with an initial context.
runTCState :: TCState a -> (Either TCError a, Ctx)
runTCState = runPartialState emptyCtx

runTEState :: TEState a -> Ctx -> (Either TEError a, TCtx)
runTEState m ctx = runState (runExceptT m) (ctx2TCtx ctx)

-- | Infer the type of a variable by its name.
inferVar :: FI R.Name ->> FI T.Ty
inferVar (FI p x) = get
  >>= \Ctx { vars, globalVars } -> case x `lookup` vars of
    Just ty -> pure $ FI p ty
    Nothing -> case x `lookup` globalVars of
      Just ty -> pure $ FI p ty
      Nothing -> throwError $ UnboundVar (p :| x)

newVar :: R.Name -> T.Ty ->> T.Ty
newVar x ty = do
  ctx <- get
  let vars' = insert x ty (vars ctx)
  put ctx { vars = vars' }
  pure ty

newTVar :: R.Name -> T.Constr ->> T.Ty
newTVar x k = do
  ctx <- get
  let tvars' = insert x k (tvars ctx)
  put ctx { tvars = tvars' }
  return . T.TyVar . length $ tvars ctx

addBot :: Int -> T.Ty ->> ()
addBot i bot = do
  tvars' <- gets tvars
  let f _ constr = Just
        $ if locked constr
          then constr
          else constr { bots = bot:bots constr }
  let updated = updateAt f i tvars'
  modify $ \ctx -> ctx { tvars = updated }

addTop :: Int -> T.Ty ->> ()
addTop i top = do
  tvars' <- gets tvars
  let f _ constr = Just
        $ if locked constr
          then constr
          else constr { tops = top:tops constr }
  let updated = updateAt f i tvars'
  modify $ \ctx -> ctx { tvars = updated }

lockConstr :: Int ->> ()
lockConstr i = do
  tvars' <- gets tvars
  let f _ constr = Just $ constr { locked = True }
  let updated = updateAt f i tvars'
  modify $ \ctx -> ctx { tvars = updated }

ctx2TCtx :: Ctx -> TCtx
ctx2TCtx Ctx { tvars } =
  TCtx { border = Uninterpreted <$> elems tvars, types = [] }

folkEnv :: PartialState e s a -> PartialState e s a
folkEnv m = do
  env0 <- get
  res <- m
  put env0
  return res

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

data TCError = UnboundVar (FI R.Name)
             | UnboundType (FI R.Name)
             | MissingLabel (FI R.Name)
             | BadPattern (FI R.Pttrn) (FI T.Ty)
             | NonProjectableType (FI T.Ty)
             | DissatisfiedParameterCount (FI Int)
  deriving (Show)

data TEError = BadCast V.FITy V.FITy
             | NotAFunctionType V.FITy
             | NotATypeFunctionType V.FITy
             | Unimplemented (FI String)
             | BadConstraint (FI T.Constr)
             | NonDeducibleArgumentType V.FITy
             | DissatisfiedTypeParameterCount (FI Int)
  deriving (Show)
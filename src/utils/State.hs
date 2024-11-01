{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module State where

import           Control.Monad.Except (ExceptT, MonadError(throwError))
import           Control.Monad.State (MonadState(get, put), State, modify, gets)
import           Data.Map (lookup, insert, updateAt)
import           Raw as R
import           Tm as T
import           Utils
import           Val as V hiding (constrs)
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
  return . T.TyVar . subtract 1 . length $ tvars ctx

addBot :: Int -> T.Ty ->> ()
addBot i bot = do
  tvars' <- gets tvars
  let f _ constr = Just $ constr { bots = bot:bots constr }
  let updated = updateAt f i tvars'
  modify $ \ctx -> ctx { tvars = updated }

addTop :: Int -> T.Ty ->> ()
addTop i top = do
  tvars' <- gets tvars
  let f _ constr = Just $ constr { tops = top:tops constr }
  let updated = updateAt f i tvars'
  modify $ \ctx -> ctx { tvars = updated }

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

data TCError = UnboundVar (FI R.Name)
             | UnboundType (FI R.Name)
             | MissingLabel (FI R.Name)
             | BadPattern (FI R.Pttrn) (FI T.Ty)
             | DissatisfiedParameterCount (FI Int)

data TEError = BadCast V.FITy V.FITy
             | Unimplemented (FI String)
             | BadConstraint (FI T.Constr)
             | NonDeducibleArgumentType V.FITy
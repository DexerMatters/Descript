{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module State where

import           Control.Exception (throw)
import           Control.Monad.Except (ExceptT, MonadError(throwError))
import           Control.Monad.State (MonadState(get, put), State, modify, gets)
import           Data.Map (lookup, elemAt)
import           Data.Map.Lazy (insert)
import           Raw as R
import           Tm as T
import           Utils
import           Val as V hiding (constrs)
import           Prelude hiding (lookup)
import           Data.Functor ((<&>))
import           Data.Map (updateAt)

--------------------------------------------------------------------------------
-- States
--------------------------------------------------------------------------------

type PartialState e s a = ExceptT e (State s) a

type TCState a = PartialState TCErrors Ctx a

type (->>) a b = a -> TCState b

--------------------------------------------------------------------------------
-- Context
--------------------------------------------------------------------------------

data Ctx = Ctx { vars :: R.Name |-> T.Ty
               , tvars :: R.Name |-> T.Constr
               , rcdSyms :: [R.Name]
               , globalTypes :: R.Name |-> T.Ty
               , globalVars :: R.Name |-> T.Ty
               }

-- | Infer the type of a variable by its name.
inferVar :: FI R.Name ->> FI T.Ty
inferVar (FI p x) = get
  >>= \Ctx { vars, globalVars } -> case x `lookup` vars of
    Just ty -> pure $ FI p ty
    Nothing -> case x `lookup` globalVars of
      Just ty -> pure $ FI p ty
      Nothing -> throwError $ UnboundVar (p :| x)

newVar :: R.Name -> T.Ty -> TCState T.Ty
newVar x ty = do
  ctx <- get
  let vars' = insert x ty (vars ctx)
  put ctx { vars = vars' }
  pure ty

newTVar :: R.Name -> T.Constr -> TCState T.Ty
newTVar x k = do
  ctx <- get
  let tvars' = insert x k (tvars ctx)
  put ctx { tvars = tvars' }
  return . T.TyVar . subtract 1 . length $ tvars ctx

addBot :: Int -> T.Ty ->> ()
addBot i bot = do
  tvars' <- gets tvars
  let f _ T.Constr { tops, bots } = Just $ T.Constr tops (bot:bots)
  let updated = updateAt f i tvars'
  modify $ \ctx -> ctx { tvars = updated }

addTop :: Int -> T.Ty ->> ()
addTop i top = do
  tvars' <- gets tvars
  let f _ T.Constr { tops, bots } = Just $ T.Constr (top:tops) bots
  let updated = updateAt f i tvars'
  modify $ \ctx -> ctx { tvars = updated }

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

data TCErrors = UnboundVar (FI R.Name)
              | UnboundType (FI R.Name)
              | BadPattern (FI R.Pttrn) (FI T.Ty)
              | DissatisfiedParameterCount (FI Int)
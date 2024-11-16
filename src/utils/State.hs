{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module State where

import           Prelude hiding (lookup)
import           Control.Monad.Except (ExceptT, runExceptT)
import           Control.Monad.State
import qualified Val as V
import qualified Tm as T
import           Utils
import           Data.Sequence ((|>), (!?), update, fromList)
import           Data.Maybe (fromJust)
import           Control.Monad (unless)

--------------------------------------------------------------------------------
-- States
--------------------------------------------------------------------------------

type EnvState s e a = ExceptT e (State s) a

runEnvState :: s -> EnvState s e a -> (Either e a, s)
runEnvState s f = runState (runExceptT f) s

isolate :: EnvState s e a -> EnvState s e a
isolate f = do
  s0 <- get
  a <- f
  put s0
  return a

type TmState a = EnvState T.Ctx T.TError a

runTmState :: TmState a -> (Either T.TError a, T.Ctx)
runTmState = runEnvState
  T.Ctx { T.vars = fromList [], T.constrs = fromList [] }

putVar :: Name -> T.Ty -> TmState Int
putVar x t = do
  vars <- gets T.vars
  let i = length vars
  modify $ \s -> s { T.vars = vars |> (x, t) }
  return i

newTyVar :: TmState Int
newTyVar = do
  constrs <- gets T.constrs
  let i = length constrs
  modify $ \s -> s { T.constrs = constrs |> T.Constr [] False }
  return i

restrict :: Constraint T.Ty -> Int -> TmState ()
restrict c i = do
  constrs <- gets T.constrs
  let constr = fromJust $ constrs !? i
  let constr' = constr { T.elems = c:T.elems constr }
  unless (T.locked constr)
    $ modify
    $ \s -> s { T.constrs = update i constr' constrs }

lockConstr :: Int -> TmState ()
lockConstr i = do
  constrs <- gets T.constrs
  let constr = fromJust $ constrs !? i
  let constr' = constr { T.locked = True }
  modify $ \s -> s { T.constrs = update i constr' constrs }

type ValState a = EnvState V.Ctx V.TError a

runValState :: T.Ctx -> ValState a -> (Either V.TError a, V.Ctx)
runValState ctx = runEnvState (liftCtx ctx)

putTypes :: [V.Ty] -> ValState ()
putTypes tys = do
  types <- gets V.types
  modify $ \s -> s { V.types = types <> fromList tys }

liftCtx :: T.Ctx -> V.Ctx
liftCtx T.Ctx { T.constrs = constrs } =
  V.Ctx { V.types = fromList [], V.constrs = T.elems <$> constrs }


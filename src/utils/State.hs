{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module State where

import           Prelude hiding (lookup)
import           Control.Monad.Except (ExceptT, runExceptT
                                     , MonadError(throwError))
import           Control.Monad.State
import qualified Val as V
import qualified Tm as T
import           Utils
import           Data.Sequence ((|>), (!?), update, fromList)
import           Data.Maybe (fromJust, isNothing)
import           Control.Monad (unless)
import qualified Raw as R
import           Errors (VTError, RTError, ProgError(..))

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

isolateWith :: s -> EnvState s e a -> EnvState s e a
isolateWith s f = do
  s0 <- get
  put s
  a <- f
  put s0
  return a

type TmState a = EnvState T.Ctx RTError a

runTmState :: R.Symbols -> TmState a -> (Either RTError a, T.Ctx)
runTmState s = runEnvState
  T.Ctx { T.vars = fromList [], T.constrs = fromList [], T.symbols = s }

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

newTyVarWithConstr :: [Constraint T.Ty] -> TmState Int
newTyVarWithConstr cs = do
  constrs <- gets T.constrs
  let i = length constrs
  modify $ \s -> s { T.constrs = constrs |> T.Constr cs True }
  return i

restrict :: Constraint T.Ty -> Int -> TmState ()
restrict c i = do
  constrs <- gets T.constrs
  let constr = constrs !? i
  case constr of
    Just c' -> do
      let constr' = c' { T.elems = c:T.elems c' }
      unless (T.locked c')
        $ modify
        $ \s -> s { T.constrs = update i constr' constrs }
    Nothing -> return ()

lockConstr :: Int -> TmState ()
lockConstr i = do
  constrs <- gets T.constrs
  let constr = fromJust $ constrs !? i
  let constr' = constr { T.locked = True }
  modify $ \s -> s { T.constrs = update i constr' constrs }

type ValState a = EnvState V.Ctx VTError a

runValState :: T.Ctx -> ValState a -> (Either VTError a, V.Ctx)
runValState ctx = runEnvState (liftCtx ctx)

putTypes :: [V.Ty] -> ValState ()
putTypes tys = do
  types' <- gets V.types
  modify $ \s -> s { V.types = types' <> fromList tys }

liftCtx :: T.Ctx -> V.Ctx
liftCtx T.Ctx { T.constrs = constrs } =
  V.Ctx { V.types = fromList [], V.constrs = T.elems <$> constrs }

type ProgState a = EnvState R.Symbols ProgError a

putTermDef :: Name -> R.Tm -> ProgState ()
putTermDef x tm = do
  -- Check if the term is already defined
  terms' <- gets R.terms
  unless (isNothing $ terms' !!? x) $ throwError $ DuplicatedValueDefinition x
  modify $ \s -> s { R.terms = terms' |> (x, tm) }

putTypeDef :: Name -> R.Ty -> ProgState ()
putTypeDef x ty = do
  -- Check if the type is already defined
  types' <- gets R.types
  unless (isNothing $ types' !!? x) $ throwError $ DuplicatedTypeDefinition x
  modify $ \s -> s { R.types = types' |> (x, ty) }

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module State where

import           Prelude hiding (lookup)
import           Control.Monad.Except (ExceptT, runExceptT
                                     , MonadError(throwError))
import           Control.Monad.State
import qualified Val as V
import qualified Tm as T
import           Utils
import           Data.Sequence ((|>), (!?), update, fromList, Seq(Empty))
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
  T.Ctx { T.vars = fromList []
        , T.constrs = fromList []
        , T.symbols = s
        , T.level = 0
        }

freshIndex :: TmState Int
freshIndex = do
  l <- gets T.level
  i <- gets (length . T.constrs)
  return $ i - l

updateLevel :: TmState ()
updateLevel = do
  i <- gets (length . T.constrs)
  modify $ \s -> s { T.level = i }

putVar :: Name -> T.Ty -> TmState Int
putVar x t = do
  vars <- gets T.vars
  let i = length vars
  modify $ \s -> s { T.vars = vars |> (x, t) }
  return i

newTyVar :: TmState (Int, Int)
newTyVar = do
  constrs <- gets T.constrs
  i <- freshIndex
  modify $ \s -> s { T.constrs = constrs |> T.Constr [] False }
  return (length constrs, i)

newTyVarWithConstr :: [Constraint T.Ty] -> TmState (Int, Int)
newTyVarWithConstr cs = do
  constrs <- gets T.constrs
  i <- freshIndex
  modify $ \s -> s { T.constrs = constrs |> T.Constr cs True }
  return (length constrs, i)

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

isLocked :: Int -> TmState Bool
isLocked i = do
  constrs <- gets T.constrs
  return $ T.locked $ fromJust $ constrs !? i

fresh :: Int -> ValState String
fresh i = do
  freshMap <- gets V.fresh
  case freshMap !!? i of
    Just j  -> return $ "τ" ++ show j
    Nothing -> do
      let j = length freshMap
      modify $ \s -> s { V.fresh = freshMap |> (i, j) }
      return $ "τ" ++ show j

type ValState a = EnvState V.Ctx VTError a

runValState :: T.Ctx -> ValState a -> (Either VTError a, V.Ctx)
runValState ctx = runEnvState (liftCtx ctx)

putTypes :: [V.Ty] -> ValState ()
putTypes tys = do
  types' <- gets V.types
  modify $ \s -> s { V.types = types' <> fromList tys }

liftCtx :: T.Ctx -> V.Ctx
liftCtx T.Ctx { T.constrs = constrs } =
  V.Ctx { V.types = fromList []
        , V.constrs = T.elems <$> constrs
        , V.fresh = Empty
        }

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

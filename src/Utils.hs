{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeOperators #-}

module Utils where

import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Control.Monad ((>=>))
import qualified Raw as R
import qualified Tm as T
import Prelude hiding ((.))

data Env = Env {vars :: [(Name, T.Ty)], tvars :: [(TName, T.Constr)]}

type TName = String

type Name = String

type Lvl = Int

data TCError
  = UnboundVariable
  | UnboundTypeVariable
  | MultipleAnnotation
  | UndefinedPattern
  | IncorrectParameterCount

newtype TCArrow a b = TCArrow {runTCArrow :: (Env, a) -> Either TCError (Env, b)}

type (->>) = TCArrow

instance Category TCArrow where
  id = TCArrow Right
  (TCArrow f) . (TCArrow g) = TCArrow $ g >=> f

instance Arrow TCArrow where
  arr f = TCArrow $ \(env, x) -> Right (env, f x)
  first (TCArrow f) = TCArrow $ \(env, (x, y)) -> do
    (env', x') <- f (env, x)
    return (env', (x', y))

pureA :: ((Env, a) -> Either TCError (Env, b)) -> a ->> b
pureA = TCArrow

fail :: TCError -> a ->> b
fail e = pureA $ \_ -> Left e

constA :: b -> a ->> b
constA x = pureA $ \(env, _) -> Right (env, x)

setEnv :: Env ->> ()
setEnv = pureA $ \(_, env) -> Right (env, ())

getEnv :: a ->> Env
getEnv = pureA $ \(env, _) -> Right (env, env)

modifyEnv :: (Env -> Env) ->> ()
modifyEnv = pureA $ \(env, f) -> Right (f env, ())

var :: Name ->> T.Ty
var = pureA $ \(env, x) -> case lookup x (vars env) of
  Just ty -> Right (env, ty)
  Nothing -> Left UnboundVariable

constr :: TName ->> T.Constr
constr = pureA $ \(env, x) -> case lookup x (tvars env) of
  Just c -> Right (env, c)
  Nothing -> Left UnboundTypeVariable

setConstr :: (TName, T.Constr) ->> ()
setConstr = proc (x, c) -> do
  modifyEnv -< \env -> env {tvars = go x c (tvars env)}
  where
    go _ _ [] = []
    go x c ((y, _) : ys)
      | x == y = (x, c) : ys
      | otherwise = (y, c) : go x c ys

newVar :: (R.Name, T.Ty) ->> ()
newVar = proc (x, ty) -> do
  modifyEnv -< \env -> env {vars = (x, ty) : vars env}

newTVar :: R.Name ->> ()
newTVar = proc x -> do
  modifyEnv -< \env -> env {tvars = (x, T.Constr [] []) : tvars env}

addBot :: (TName, T.Ty) ->> ()
addBot = proc (x, ty) -> do
  c <- constr -< x
  setConstr -< (x, c {T.bots = ty : T.bots c})

addTop :: (TName, T.Ty) ->> ()
addTop = proc (x, ty) -> do
  c <- constr -< x
  setConstr -< (x, c {T.tops = ty : T.tops c})
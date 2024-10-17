{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Utils where

import Control.Arrow (Arrow (..), ArrowChoice (left), returnA, (>>>), (>>^))
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
  | UndefinedField
  | NotAFunction
  | NotARecord
  | UndefinedBehavior
  deriving (Show)

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

instance ArrowChoice TCArrow where
  left (TCArrow f) = TCArrow $ \case
    (env, Left x) -> do
      (env', x') <- f (env, x)
      return (env', Left x')
    (env, Right y) -> Right (env, Right y)

runTC :: TCArrow a b -> a -> Either TCError b
runTC f = fmap snd . runTCArrow f . (Env {vars = [], tvars = []},)

pureA :: ((Env, a) -> Either TCError (Env, b)) -> a ->> b
pureA = TCArrow

throw :: TCError -> a ->> b
throw e = pureA $ \_ -> Left e

fmapA :: (a ->> b) -> ([a] ->> [b])
fmapA f = proc x -> case x of
  [] -> returnA -< []
  y : ys -> do
    f' <- f -< y
    zs <- fmapA f -< ys
    returnA -< f' : zs

fmapA' :: (a ->> b) -> ([a] ->> ())
fmapA' f = proc x -> case x of
  [] -> returnA -< ()
  y : ys -> do
    f -< y
    fmapA' f -< ys

constA :: b -> a ->> b
constA x = pureA $ \(env, _) -> Right (env, x)

setEnv :: Env ->> ()
setEnv = pureA $ \(_, env) -> Right (env, ())

getEnv :: () ->> Env
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

getTConstr :: Int ->> T.Constr
getTConstr = pureA $ \(env, i) -> Right (env, snd (tvars env !! i))

setTConstr :: (Int, T.Constr) ->> ()
setTConstr = proc (i, c) -> do
  modifyEnv -< \env -> env {tvars = go i c (tvars env)}
  where
    go _ _ [] = []
    go i c ((x, c') : xs)
      | i == 0 = (x, c) : xs
      | otherwise = (x, c') : go (i - 1) c xs

getTName :: Int ->> TName
getTName = pureA $ \(env, i) -> Right (env, fst (tvars env !! i))

setConstr :: (TName, T.Constr) ->> ()
setConstr = proc (x, c) -> do
  modifyEnv -< \env -> env {tvars = go x c (tvars env)}
  where
    go _ _ [] = []
    go x c ((y, _) : ys)
      | x == y = (x, c) : ys
      | otherwise = (y, c) : go x c ys

newVar :: (R.Name, T.Ty) ->> T.Ty
newVar = proc (x, ty) -> do
  modifyEnv -< \env -> env {vars = (x, ty) : vars env}
  returnA -< ty

newTVar :: R.Name ->> ()
newTVar = proc x -> do
  modifyEnv -< \env -> env {tvars = (x, T.Constr [] []) : tvars env}

addBot :: (Int, T.Ty) ->> ()
addBot = proc (i, ty) -> do
  c <- getTConstr -< i
  setTConstr -< (i, c {T.bots = ty : T.bots c})

addTop :: (Int, T.Ty) ->> ()
addTop = proc (i, ty) -> do
  c <- getTConstr -< i
  setTConstr -< (i, c {T.tops = ty : T.tops c})
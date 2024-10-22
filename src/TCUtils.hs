{-# LANGUAGE Arrows #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}

module TCUtils where

import Control.Arrow (returnA)
import Debug.Trace (trace)
import qualified Tm as T
import Utils

data Env = Env {vars :: [(Name, T.Ty)], tvars :: [(TName, T.Constr)]} deriving (Show)

type TName = String

type Name = String

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

type (->>) = PartialArrow Env TCError

instance Emptyness Env where
  empty :: Env
  empty = Env [] []

var :: Name ->> T.Ty
var = PartialArrow $ \(env, x) -> case lookup x (vars env) of
  Just ty -> Right (env, ty)
  Nothing -> Left UnboundVariable

constr :: TName ->> T.Constr
constr = PartialArrow $ \(env, x) -> case lookup x (tvars env) of
  Just c -> Right (env, c)
  Nothing -> Left UnboundTypeVariable

getTConstr :: Int ->> T.Constr
getTConstr = PartialArrow $ \(env, i) -> Right (env, snd (tvars env !! i))

setTConstr :: (Int, T.Constr) ->> ()
setTConstr = proc (i, c) -> do
  modifyEnv -< \env -> env {tvars = go i c (tvars env)}
  where
    go _ _ [] = []
    go i c ((x, c') : xs)
      | i == 0 = (x, c) : xs
      | otherwise = (x, c') : go (i - 1) c xs

getTName :: Int ->> TName
getTName = PartialArrow $ \(env, i) -> Right (env, fst (tvars env !! i))

setConstr :: (TName, T.Constr) ->> ()
setConstr = proc (x, c) -> do
  modifyEnv -< \env -> env {tvars = go x c (tvars env)}
  where
    go _ _ [] = []
    go x c ((y, _) : ys)
      | x == y = (x, c) : ys
      | otherwise = (y, c) : go x c ys

newVar :: (Name, T.Ty) ->> T.Ty
newVar = proc (x, ty) -> do
  modifyEnv -< \env -> env {vars = (x, ty) : vars env}
  returnA -< ty

newTVar :: Name ->> ()
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
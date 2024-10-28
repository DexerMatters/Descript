{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module TEUtils where

import qualified TCUtils as TC
import Utils
import qualified Val as V

type Pos = (Int, Int)

data TEError
  = ImproperBound Pos
  | BadCast Pos (String, String)
  | NotAFunction Pos
  | UndefinedMacro Pos String
  deriving (Show)

type (->>) = PartialArrow V.Env TEError

addType :: FI V.Ty ->> ()
addType = proc t -> do
  modifyEnv -< \env -> env {V.types = t : V.types env}

getType :: Int ->> FI V.Ty
getType = PartialArrow $ \(env, i) -> Right (env, V.types env !! i)

getConstrs :: Int ->> V.ConstrState
getConstrs = PartialArrow $ \(env, i) -> Right (env, V.constrs env !! i)

liftEnv :: TC.Env -> V.Env
liftEnv env = V.Env {V.types = [], V.constrs = Uninterpreted . snd <$> TC.tvars env}
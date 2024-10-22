{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeOperators #-}

module TEUtils where

import Control.Arrow (returnA)
import qualified TCUtils as TC
import qualified Tm
import Utils
import qualified Val as V

data TEError
  = ImproperBound
  | BadCast
  | NotAFunction
  deriving (Show)

type (->>) = PartialArrow V.Env TEError

addType :: V.Ty ->> ()
addType = proc t -> do
  modifyEnv -< \env -> env {V.types = t : V.types env}

getType :: Int ->> V.Ty
getType = PartialArrow $ \(env, i) -> Right (env, V.types env !! i)

getConstrs :: Int ->> V.ConstrState
getConstrs = PartialArrow $ \(env, i) -> Right (env, V.constrs env !! i)

liftEnv :: TC.Env -> V.Env
liftEnv env = V.Env {V.types = [], V.constrs = Uninterpreted . snd <$> TC.tvars env}
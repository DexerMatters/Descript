{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeOperators #-}

module TEUtils where

import qualified TCUtils as TC
import Utils
import Val (Env (..))
import qualified Val as V

data TEError
  = ImproperBound
  | BadCast
  | NotAFunction

type (->>) = PartialArrow Env TEError

addType :: V.Ty ->> ()
addType = proc t -> do
  modifyEnv -< \env -> env {types = t : types env}

getType :: Int ->> V.Ty
getType = PartialArrow $ \(env, i) -> Right (env, types env !! i)

getBorder :: Int ->> V.Border
getBorder = PartialArrow $ \(env, i) -> Right (env, borders env !! i)

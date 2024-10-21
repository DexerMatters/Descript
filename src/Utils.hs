{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Utils where

import Control.Arrow (Arrow (..), ArrowChoice (left), returnA)
import Control.Category (Category (..))
import Control.Monad ((>=>))
import Prelude hiding ((.))

newtype PartialArrow s e a b = PartialArrow {runPartialArrow :: (s, a) -> Either e (s, b)}

class Emptyness a where
  empty :: a

class PrettyShow a where
  prettyShow :: a -> String

instance Category (PartialArrow s e) where
  id = PartialArrow Right
  (PartialArrow f) . (PartialArrow g) = PartialArrow $ g >=> f

instance Arrow (PartialArrow s e) where
  arr f = PartialArrow $ \(env, x) -> Right (env, f x)
  first (PartialArrow f) = PartialArrow $ \(env, (x, y)) -> do
    (env', x') <- f (env, x)
    return (env', (x', y))

instance ArrowChoice (PartialArrow s e) where
  left (PartialArrow f) = PartialArrow $ \case
    (env, Left x) -> do
      (env', x') <- f (env, x)
      return (env', Left x')
    (env, Right y) -> Right (env, Right y)

runPartially :: (Emptyness s) => PartialArrow s e a b -> a -> Either e (b, s)
runPartially f x = case runPartialArrow f (empty, x) of
  Left e -> Left e
  Right (env, y) -> Right (y, env)

throw :: e -> PartialArrow s e a b
throw e = PartialArrow $ \_ -> Left e

fmapA :: PartialArrow s e a b -> PartialArrow s e [a] [b]
fmapA f = proc x -> case x of
  [] -> returnA -< []
  y : ys -> do
    f' <- f -< y
    zs <- fmapA f -< ys
    returnA -< f' : zs

fmapA' :: PartialArrow s e a b -> PartialArrow s e [a] ()
fmapA' f = proc x -> case x of
  [] -> returnA -< ()
  y : ys -> do
    f -< y
    fmapA' f -< ys

setEnv :: PartialArrow s e s ()
setEnv = PartialArrow $ \(_, env) -> Right (env, ())

getEnv :: PartialArrow s e () s
getEnv = PartialArrow $ \(env, _) -> Right (env, env)

modifyEnv :: PartialArrow s e (s -> s) ()
modifyEnv = PartialArrow $ \(env, f) -> Right (f env, ())

genTVarName :: Int -> String
genTVarName i
  | i < 26 = show $ ['a' .. 'z'] !! i
  | otherwise = "t" ++ show i
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Utils where

import Control.Arrow (Arrow (..), ArrowChoice (left), returnA)
import Control.Category (Category (..))
import Control.Monad ((>=>))
import Debug.Trace (trace)
import GHC.Arr (Array)
import Prelude hiding ((.))

newtype PartialArrow s e a b = PartialArrow {runPartialArrow :: (s, a) -> Either e (s, b)}

data EvalState a b = Uninterpreted a | Interpreted b

data FI a = FI {fi :: (Int, Int), val :: a} deriving (Show)

type Arr = Array Int

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

tr :: (Show a) => a -> a
tr x = trace (show x) x

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

getMaxAWith :: PartialArrow s e (b, b) Bool -> PartialArrow s e [b] (Maybe b)
getMaxAWith f = proc xs -> case xs of
  [] -> returnA -< Nothing
  x : xs' -> do
    y <- getMaxAWith f -< xs'
    case y of
      Nothing -> returnA -< Just x
      Just y' -> do
        b <- f -< (x, y')
        returnA -< if b then Just x else Just y'

instance (PrettyShow a) => PrettyShow [a] where
  prettyShow [] = "Empty"
  prettyShow xs =
    replicate 5 '-' ++ "\n" ++ unlines (prettyShow <$> xs) ++ replicate 5 '-'

instance (PrettyShow a, PrettyShow b) => PrettyShow (a, b) where
  prettyShow (x, y) = ">" ++ prettyShow x ++ "\t\t" ++ prettyShow y

instance (PrettyShow a, PrettyShow b) => PrettyShow (EvalState a b) where
  prettyShow (Uninterpreted x) = prettyShow x ++ "\t Uninterpreted"
  prettyShow (Interpreted x) = prettyShow x

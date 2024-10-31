{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Utils
    ( FI(..)
    , EvalState(..)
    , Tril(..)
    , evalStateM
    , fromInterpreted
    , replace
    , tril
    , toBool
    , fromBool
    , allT
    , anyT
    , (&&&)
    , (|||)
    , type (|->)
    , pattern (:<>:)
    , pattern (:|)
    , includeByM
    , trEq) where

import           Data.Map (Map)

data EvalState a b = Uninterpreted a
                   | Interpreted b
  deriving (Show)

data FI a = FI { pos :: (Int, Int), val :: a }

-- | Trilean data type
data Tril = True'
          | False'
          | Unknown

instance (Show a) => Show (FI a) where
  show (FI _ x) = show x

instance Functor FI where
  fmap f (FI p x) = FI p (f x)

pattern (:|) :: (Int, Int) -> a -> FI a
pattern (:|) a b <- FI a b
  where
    p :| x = FI p x

infixr 9 :|

pattern (:<>:) :: a -> b -> (a, b)
pattern (:<>:) a b = (a, b)

infixr 6 :<>:

type (|->) = Map

-- | Convert a Tril to a Bool
toBool :: Tril -> Bool
toBool True' = True
toBool False' = False
toBool Unknown = False

-- | Convert a Bool to a Tril
fromBool :: Bool -> Tril
fromBool True = True'
fromBool False = False'

-- | Trilean logic
tril :: a -> a -> a -> Tril -> a
tril t _ _ True' = t
tril _ f _ False' = f
tril _ _ u Unknown = u

-- | Trilean logic-AND operator
(&&&) :: Tril -> Tril -> Tril
(&&&) True' True' = True'
(&&&) Unknown _ = Unknown
(&&&) _ Unknown = Unknown
(&&&) _ _ = False'

-- | Trilean logic-OR operator
(|||) :: Tril -> Tril -> Tril
(|||) False' False' = False'
(|||) Unknown _ = Unknown
(|||) _ Unknown = Unknown
(|||) _ _ = True'

allT :: [Tril] -> Tril
allT = foldr (&&&) True'

anyT :: [Tril] -> Tril
anyT = foldr (|||) False'

fromInterpreted :: EvalState a b -> b
fromInterpreted (Interpreted b) = b
fromInterpreted _ = error "impossible"

evalStateM :: Monad m => (a -> m b) -> EvalState a b -> m (EvalState a b)
evalStateM f (Uninterpreted a) = Interpreted <$> f a
evalStateM _ b = pure b

replace :: [a] -> Int -> a -> [a]
replace xs i e = case splitAt i xs of
  (before, _:after) -> before ++ e:after
  _ -> xs

-- | Check if the second list is the subset of the first list by a monad function
includeByM :: Monad m => (a -> b -> m Tril) -> [a] -> [b] -> m Tril
includeByM f = go
  where
    go _ [] = return True'
    go [] _ = return False'
    go (x:xs') (y:ys') = do
      result <- f x y
      case result of
        False'  -> return False'
        Unknown -> return Unknown
        True'   -> go xs' ys'

trEq :: Eq a => a -> a -> Tril
trEq x y = fromBool $ x == y
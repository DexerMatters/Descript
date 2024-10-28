{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Utils
    ( FI(..)
    , EvalState(..)
    , type (|->)
    , pattern (:<>:)
    , pattern (:|)) where

import           Data.Map (Map)
import           Prelude hiding ((.))

data EvalState a b = Uninterpreted a
                   | Interpreted b
  deriving (Show)

data FI a = FI { pos :: (Int, Int), val :: a }

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

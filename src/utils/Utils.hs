{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{-# LANGUAGE TupleSections #-}

module Utils where

import           Data.Sequence (Seq(..))
import           Data.Graph (Tree)
import           Data.Tree (Tree(Node))

-- | Aliases for common types
type Name = String

type Label = String

data Constraint a = Top a
                  | Bot a
  deriving (Show)

instance Functor Constraint where
  fmap f (Top a) = Top (f a)
  fmap f (Bot a) = Bot (f a)

unwrap :: Constraint a -> a
unwrap (Top a) = a
unwrap (Bot a) = a

-- | Sequence-based map
type (|->) k v = Seq (k, v)

ran :: k |-> v -> Seq v
ran = fmap snd

(!!!) :: (Eq k) => k |-> v -> k -> v
(!!!) ((k', v) :<| xs) k
  | k == k' = v
  | otherwise = xs !!! k
(!!!) Empty _ = error "Key not found"

(!!?) :: (Eq k) => k |-> v -> k -> Maybe v
(!!?) ((k', v) :<| xs) k
  | k == k' = Just v
  | otherwise = xs !!? k
(!!?) Empty _ = Nothing

(!!?~) :: (Eq k) => k |-> v -> k -> Maybe v
(!!?~) (xs :|> (k', v)) k
  | k == k' = Just v
  | otherwise = xs !!? k
(!!?~) Empty _ = Nothing

secondM :: Applicative f => (b -> f c) -> (a, b) -> f (a, c)
secondM f (a, b) = (a, ) <$> f b

-- | Lift a constraint
liftConstraint :: Functor m => Constraint (m a) -> m (Constraint a)
liftConstraint (Top ma) = Top <$> ma
liftConstraint (Bot ma) = Bot <$> ma

type MaybeTree a = Tree (Maybe a)

flattenMaybe :: MaybeTree a -> [a]
flattenMaybe (Node (Just a) ts) = a:concatMap flattenMaybe ts
flattenMaybe (Node Nothing ts) = concatMap flattenMaybe ts
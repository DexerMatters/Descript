{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{-# LANGUAGE TupleSections #-}

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
    , insert
    , insert'
    , updateAt
    , lookup
    , lookupIndex
    , elems
    , (&&&)
    , (|||)
    , type (|->)
    , pattern (:*:)
    , pattern (:<*>:)
    , pattern (:<*:)
    , pattern (:*>:)
    , pattern (:|)
    , includeByM
    , trEq) where

import           Prelude hiding (lookup)

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
pattern (:|) a b = FI a b

infixr 8 :|

pattern (:*:) :: a -> b -> (a, b)
pattern (:*:) a b = (a, b)

pattern (:<*>:) :: a1 -> a2 -> (FI a1, FI a2)
pattern (:<*>:) a b <- FI _ a :*: FI _ b
  where
    (:<*>:) a b = FI undefined a :*: FI undefined b

pattern (:<*:) :: a -> b -> (FI a, b)
pattern (:<*:) a b <- FI _ a :*: b
  where
    (:<*:) a b = FI undefined a :*: b

pattern (:*>:) :: a1 -> a2 -> (a1, FI a2)
pattern (:*>:) a b <- a :*: FI _ b
  where
    (:*>:) a b = a :*: FI undefined b

infixr 6 :*:, :<*:, :<*>:, :*>:

type a |->  b = [(a, b)]

insert :: a -> b -> a |-> b -> a |-> b
insert a b = ((a, b):)

insert' :: a -> b -> a |-> b -> a |-> b
insert' a b = (++ [(a, b)])

updateAt :: (k -> a -> Maybe a) -> Int -> k |-> a -> k |-> a
updateAt f = aux
  where
    aux _ [] = []
    aux 0 ((k, v):xs') = case f k v of
      Just v' -> (k, v'):xs'
      Nothing -> xs'
    aux j (x:xs') = x:aux (j - 1) xs'

lookup :: Eq a => a -> a |-> b -> Maybe b
lookup _ [] = Nothing
lookup a ((a', b):xs) = if a == a'
                        then Just b
                        else a `lookup` xs

lookupIndex :: Eq a => a -> a |-> b -> Maybe Int
lookupIndex _ [] = Nothing
lookupIndex a ((a', _):xs) = if a == a'
                             then Just 0
                             else (1 +) <$> lookupIndex a xs

elems :: k |-> a -> [a]
elems = map snd

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
includeByM :: Monad m => (a -> b -> m Bool) -> [a] -> [b] -> m Bool
includeByM f = go
  where
    go _ [] = return True
    go [] _ = return False
    go (x:xs') (y:ys') = do
      b <- f x y
      if b
        then go xs' ys'
        else return False

trEq :: Eq a => a -> a -> Tril
trEq x y = fromBool $ x == y


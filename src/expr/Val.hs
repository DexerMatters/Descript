{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{-# LANGUAGE TypeOperators #-}

module Val where

import           Data.List (intercalate)
import           Data.Sequence (Seq)
import qualified Tm as T (Ty(..))
import           Raw (Prim)
import           Utils

type Border = (Ty, Ty)

data Ctx = Ctx { types :: Seq Ty, constrs :: Seq [Constraint T.Ty] }
  deriving (Show)

data Closure = Closure { env :: Ctx, body :: T.Ty }
  deriving (Show)

data Ty = TyVar Int
        | TyPrim Prim
        | TyArrow [Ty] Ty
        | TyTuple [Ty]
        | TyRcd [(Name, Ty)]
        | TyLam Int Closure
        | TyApp Ty [Ty]

instance Show Ty where
  show (TyVar i) = show i
  show (TyPrim p) = show p
  show (TyArrow tys ty) =
    "(" ++ intercalate ", " (map show tys) ++ ") -> " ++ show ty
  show (TyTuple tys) = "(" ++ intercalate ", " (map show tys) ++ ")"
  show (TyRcd tys) =
    "{" ++ intercalate ", " (map (\(l, t) -> l ++ ": " ++ show t) tys) ++ "}"
  show (TyLam i (Closure _ tms)) =
    "Forall(" ++ show i ++ ")" ++ "." ++ "<" ++ show tms ++ ">"
  show (TyApp ty tys) =
    show ty ++ "<" ++ intercalate ", " (map show tys) ++ ">"

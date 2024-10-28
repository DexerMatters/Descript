{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Val where

import Data.List (intercalate)
import Raw (Name)
import Tm (Constr, Prim)
import qualified Tm as T (Ty (..))
import Utils (EvalState, FI)

type Border = (FITy, FITy)

type ConstrState = EvalState Tm.Constr Border

type FITy = FI Ty

data Env = Env {types :: [FITy], constrs :: [ConstrState]}

data Closure = Closure Env (FI T.Ty)

data Ty
  = TyVar Int
  | TyPrim Prim
  | TyArrow [FITy] FITy
  | TyTuple [FITy]
  | TyRcd [(Name, FITy)]
  | TyLam Int Closure
  | TyTop
  | TyBot

instance Show Ty where
  show (TyVar i) = show i
  show (TyPrim p) = show p
  show (TyArrow tys ty) = "(" ++ intercalate ", " (map show tys) ++ ") -> " ++ show ty
  show (TyTuple tys) = "(" ++ intercalate ", " (map show tys) ++ ")"
  show (TyRcd tys) = "{" ++ intercalate ", " (map (\(l, t) -> l ++ ": " ++ show t) tys) ++ "}"
  show (TyLam i (Closure _ tms)) = "Forall(" ++ show i ++ ")" ++ "." ++ "<" ++ show tms ++ ">"
  show TyTop = "Top"
  show TyBot = "Bot"

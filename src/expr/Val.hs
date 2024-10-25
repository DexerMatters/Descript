{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Val where

import Data.List (intercalate)
import Raw (Name)
import Tm (Constr, Prim)
import qualified Tm as T (Ty (..))
import Utils (EvalState, FI, PrettyShow (prettyShow), genTVarName)

type Border = (FITy, FITy)

type ConstrState = EvalState Tm.Constr Border

type FITy = FI Ty

data Env = Env {types :: [FITy], constrs :: [ConstrState]} deriving (Show)

data Closure = Closure Env (FI T.Ty) deriving (Show)

data Ty
  = TyVar Int
  | TyPrim Prim
  | TyArrow [FITy] FITy
  | TyTuple [FITy]
  | TyRcd [(Name, FITy)]
  | TyLam Int Closure
  | TyTop
  | TyBot
  deriving (Show)

instance PrettyShow Ty where
  prettyShow (TyVar i) = genTVarName i
  prettyShow (TyPrim p) = prettyShow p
  prettyShow (TyArrow tys ty) = "(" ++ intercalate ", " (map prettyShow tys) ++ ") -> " ++ prettyShow ty
  prettyShow (TyTuple tys) = "(" ++ intercalate ", " (map prettyShow tys) ++ ")"
  prettyShow (TyRcd tys) = "{" ++ intercalate ", " (map (\(l, t) -> l ++ ": " ++ prettyShow t) tys) ++ "}"
  prettyShow (TyLam i (Closure _ tms)) = "Forall(" ++ show i ++ ")" ++ "." ++ "<" ++ prettyShow tms ++ ">"
  prettyShow TyTop = "Top"
  prettyShow TyBot = "Bot"

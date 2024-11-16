{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{-# LANGUAGE TypeOperators #-}

module Tm where

import           Data.List (intercalate)
import           Data.Sequence
import           Utils

data Ctx = Ctx { vars :: Name |-> Ty, constrs :: Seq Constr }
  deriving (Show)

data Ty   -- Explicit types
  = TyVar Int
  | TyPrim Prim
  | TyArrow [Ty] Ty
  | TyTuple [Ty]
  | TyRcd [(Label, Ty)]
  | TyApp Ty [Ty] [Ty]
    -- Generated types
  | TyLam Int Ty
  | TyCast Ty Ty
  | TyBiCast Ty Ty
  | TyMacro String Ty

data Prim = PrimNum
          | PrimBool
          | PrimStr
          | PrimUnit
          | PrimUDT String
  deriving (Eq)

data Pttrn = PttrnAtom String
           | PttrnAnn Pttrn Ty
           | PttrnTuple [Pttrn]
  deriving (Show)

-- Other types

data Constr = Constr { elems :: [Constraint Ty], locked :: Bool }
  deriving (Show)

instance Show Prim where
  show :: Prim -> String
  show PrimNum = "Number"
  show PrimBool = "Bool"
  show PrimStr = "String"
  show PrimUnit = "Unit"
  show (PrimUDT s) = s

instance Show Ty where
  show :: Ty -> String
  show (TyVar i) = "%T" ++ show i
  show (TyPrim p) = show p
  show (TyArrow tys ty) =
    "(" ++ intercalate ", " (map show tys) ++ ") -> " ++ show ty
  show (TyTuple tys) = "(" ++ intercalate ", " (map show tys) ++ ")"
  show (TyRcd rcd) = "Record{"
    ++ unwords (map (\(l, t) -> l ++ ": " ++ show t ++ "; ") rcd)
    ++ "}"
  show (TyApp ty _ tys) =
    show ty ++ "<" ++ intercalate ", " (map show tys) ++ ">"
  show (TyLam i ty) = "Forall(" ++ show i ++ ")" ++ "." ++ show ty
  show (TyMacro s ty) = s ++ "!(" ++ show ty ++ ")"
  show (TyCast ty1 ty2) = show ty1 ++ " !=> " ++ show ty2
  show (TyBiCast ty1 ty2) = show ty1 ++ " <=> " ++ show ty2

data TError = UnboundVar Name
            | UnboundType Name
            | MissingLabel Name
            | BadPattern Pttrn Ty
            | NonProjectableType Ty
            | DissatisfiedParameterCount Int
  deriving (Show)

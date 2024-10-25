{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Tm where

import Data.List (intercalate)
import Utils (FI, PrettyShow (prettyShow))

type FITy = FI Ty

data Ty
  = -- Explicit types
    TyVar Int
  | TyPrim Prim
  | TyArrow [FITy] FITy
  | TyTuple [FITy]
  | TyRcd [(Label, FITy)]
  | TyApp FITy [FITy]
  | -- Generated types
    TyLam Int FITy
  | TyCast FITy FITy
  | TyBiCast FITy FITy
  | TySeq [FITy]
  deriving (Show)

data Prim
  = PrimNum
  | PrimBool
  | PrimStr
  | PrimUnit
  deriving (Show, Eq)

data Pttrn
  = PttrnAtom String
  | PttrnAnn Pttrn FITy
  | PttrnTuple [Pttrn]
  deriving (Show)

-- Other types

type Label = String

data Constr = Constr {tops :: [FITy], bots :: [FITy]} deriving (Show)

emptyConstr :: Constr
emptyConstr = Constr [] []

instance PrettyShow Prim where
  prettyShow :: Prim -> String
  prettyShow PrimNum = "Number"
  prettyShow PrimBool = "Bool"
  prettyShow PrimStr = "String"
  prettyShow PrimUnit = "Unit"

instance PrettyShow Ty where
  prettyShow :: Ty -> String
  prettyShow (TyVar i) = "%T" ++ show i
  prettyShow (TyPrim p) = prettyShow p
  prettyShow (TyArrow tys ty) = "(" ++ intercalate ", " (map prettyShow tys) ++ ") -> " ++ prettyShow ty
  prettyShow (TyTuple tys) = "(" ++ intercalate ", " (map prettyShow tys) ++ ")"
  prettyShow (TyRcd rcd) = "Record{" ++ unwords (map (\(l, t) -> l ++ ": " ++ prettyShow t ++ "; ") rcd) ++ "}"
  prettyShow (TyApp ty tys) = prettyShow ty ++ "<<" ++ intercalate ", " (map prettyShow tys) ++ ">>"
  prettyShow (TyLam i ty) = "Forall(" ++ show i ++ ")" ++ "." ++ prettyShow ty
  prettyShow (TyCast ty1 ty2) = prettyShow ty1 ++ " => " ++ prettyShow ty2
  prettyShow (TyBiCast ty1 ty2) = prettyShow ty1 ++ " <=> " ++ prettyShow ty2
  prettyShow (TySeq tys) = "Sequence{" ++ intercalate ", " (map prettyShow tys) ++ "}"

instance PrettyShow Constr where
  prettyShow :: Constr -> String
  prettyShow (Constr ts bs) =
    "Tops: "
      ++ intercalate ", " (map prettyShow ts)
      ++ "\n"
      ++ " Bots: "
      ++ intercalate ", " (map prettyShow bs)
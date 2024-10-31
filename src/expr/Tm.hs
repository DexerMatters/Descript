{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{-# LANGUAGE TypeOperators #-}

module Tm where

import           Data.List (intercalate)
import           Utils
import           Data.Map (empty)

type FITy = FI Ty

type Name = String

data Ty   -- Explicit types
  = TyVar Int
  | TyPrim Prim
  | TyArrow [FITy] FITy
  | TyTuple [FITy]
  | TyRcd [(Label, FITy)]
  | TyApp FITy [FITy]
    -- Generated types
  | TyLam Int FITy
  | TyCast FITy FITy
  | TyBiCast FITy FITy
  | TyReduce FITy [FITy]
  | TyMacro String FITy

data Prim = PrimNum
          | PrimBool
          | PrimStr
          | PrimUnit
          | PrimUDT String
  deriving (Eq)

data Pttrn = PttrnAtom String
           | PttrnAnn Pttrn FITy
           | PttrnTuple [Pttrn]
  deriving (Show)

-- Other types

type Label = String

data Constr = Constr { env :: Ctx, tops :: [Ty], bots :: [Ty] }

data Ctx = Ctx { vars :: Name |-> Ty
               , tvars :: Name |-> Constr
               , rcdSyms :: [Name]
               , globalTypes :: Name |-> Ty
               , globalVars :: Name |-> Ty
               }

emptyCtx :: Ctx
emptyCtx = Ctx empty empty [] empty empty

emptyConstr :: Constr
emptyConstr = Constr emptyCtx [] []

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
  show (TyApp ty tys) =
    show ty ++ "<" ++ intercalate ", " (map show tys) ++ ">"
  show (TyLam i ty) = "Forall(" ++ show i ++ ")" ++ "." ++ show ty
  show (TyReduce ty tys) =
    show ty ++ "[" ++ intercalate ", " (map show tys) ++ "]"
  show (TyMacro s ty) = s ++ "!(" ++ show ty ++ ")"
  show (TyCast ty1 ty2) = show ty1 ++ " !=> " ++ show ty2
  show (TyBiCast ty1 ty2) = show ty1 ++ " <=> " ++ show ty2

instance Show Constr where
  show :: Constr -> String
  show (Constr _ ts bs) = "Tops: "
    ++ intercalate ", " (map show ts)
    ++ "\n"
    ++ " Bots: "
    ++ intercalate ", " (map show bs)

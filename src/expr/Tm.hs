{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{-# LANGUAGE TypeOperators #-}

module Tm where

import           Data.List (intercalate)
import           Data.Sequence
import           Utils
import qualified Raw as R

data Ctx = Ctx { vars :: Name |-> Ty
               , constrs :: Seq Constr
               , symbols :: R.Symbols
               , level :: Int
               }
  deriving (Show)

emptyCtx :: R.Symbols -> Ctx
emptyCtx s =
  Ctx { vars = fromList [], constrs = fromList [], symbols = s, level = 0 }

data Ty   -- Explicit types
  = TyVar Int {- Constraint Pointer -} Int {- Level -}
  | TyPrim R.Prim
  | TyArrow [Ty] Ty
  | TyTuple [Ty]
  | TyRcd [(Label, Ty)]
  | TyApp Ty [Ty] [Ty]
    -- Generated types
  | TyLam Int Ty
  | TyCast Ty Ty
  | TyBiCast Ty Ty
  | TyMacro String Ty

data Pttrn = PttrnAtom String
           | PttrnAnn Pttrn Ty
           | PttrnTuple [Pttrn]
  deriving (Show)

-- Other types

data Constr = Constr { elems :: [Constraint Ty], locked :: Bool }
  deriving (Show)

instance Show Ty where
  show :: Ty -> String
  show (TyVar _ l) = "%T" ++ show l
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
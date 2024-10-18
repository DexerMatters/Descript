{-# LANGUAGE InstanceSigs #-}

module Tm where

import Control.Monad (join)
import Data.List (intercalate)

data Ty
  = -- Explicit types
    TyVar Int
  | TyPrim Prim
  | TyArrow [Ty] Ty
  | TyTuple [Ty]
  | TyRcd [(Label, Ty)]
  | TyApp Ty [Ty]
  | -- Generated types
    TyLam Int Ty
  | TyCast Ty Ty
  | TyBiCast Ty Ty
  | TySeq [Ty]
  deriving (Show)

data Prim
  = PrimNum
  | PrimBool
  | PrimStr
  | PrimUnit
  deriving (Show, Eq)

data Pttrn
  = PttrnAtom String
  | PttrnAnn Pttrn Ty
  | PttrnTuple [Pttrn]
  deriving (Show)

-- Other types

type Label = String

data Constr = Constr {tops :: [Ty], bots :: [Ty]} deriving (Show)

emptyConstr :: Constr
emptyConstr = Constr [] []

-- Aux functions
(<^) :: Constr -> Ty -> Constr
(<^) (Constr ts bs) t = Constr (t : ts) bs

(<$) :: Constr -> Ty -> Constr
(<$) (Constr ts bs) t = Constr ts (t : bs)

infixr 5 <^, <$

(|-) :: t1 -> (t1 -> t2) -> t2
(|-) env f = f env

infixl 9 |-

class PrettyShow a where
  prettyShow :: a -> String

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
  prettyShow (TyApp ty tys) = prettyShow ty ++ intercalate ", " (map prettyShow tys)
  prettyShow (TyLam i ty) = "Forall(" ++ show i ++ ")" ++ "." ++ prettyShow ty
  prettyShow (TyCast ty1 ty2) = prettyShow ty1 ++ " => " ++ prettyShow ty2
  prettyShow (TyBiCast ty1 ty2) = prettyShow ty1 ++ " <=> " ++ prettyShow ty2
  prettyShow (TySeq tys) = "Sequence{" ++ intercalate ", " (map prettyShow tys) ++ "}"

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Raw where

import           Tm (Prim)
import           Utils (FI)

-- Syntax
newtype Prog = Prog [FI Def]
  deriving (Show)

type EnumField = (String, [FITy])

data Def = FuncDef Name [FI Pttrn] (Maybe (FI Ty)) (FI Tm)
         | ValDef Name (FI Tm)
         | TyLet Name (FI Ty)
         | EnumDef Name [String {- Type Variables -}] [EnumField]
  deriving (Show)

data Tm =
    Var Name
  | Lit Lit
  | Lam [FI Pttrn {- Arguments -}] (Maybe FITy {- Return -}) FITm {- Body -}
  | App FITm [FITm]
  | Let (FI Pttrn) FITm FITm
  | Cond FITm {- Pred -} FITm {- Then -} FITm {- Else -}
  | Tuple [FITm]
  | Proj FITm Label
  | Ann FITm FITy
  | Seq [FITm]
  | Rcd [(Label, FITm)]
    -- Primitive operations
  | Macro Name FITm
  deriving (Show)

data Ty   -- Explicit types
  = TyVar Name
  | TyPrim Prim
  | TyArrow [FITy] FITy
  | TyTuple [FITy]
  | TyRcd [(Label, FITy)]
  | TyApp FITy [FITy]
    -- Generated types
  | TyCast FITy FITy
  | TyLam [Name] FITy
  deriving (Show)

-- Other types
type Name = String

type Label = String

data Lit = LitNum Int
         | LitBool Bool
         | LitStr String
         | LitUnit
  deriving (Show)

data Pttrn = PttrnAtom Name
           | PttrnAnn (FI Pttrn) (FI Ty)
           | PttrnTuple [FI Pttrn]
  deriving (Show)

type FITm = FI Tm

type FITy = FI Ty

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Raw where

import Tm (Prim)
import Utils (FI)

-- Syntax
newtype Prog = Prog [Def]

data Def
  = FuncDef Name [Pttrn] (Maybe Ty) Tm
  | ValDef Name Tm
  | TyLet Name Ty
  deriving (Show)

data Tm
  = Var Name
  | Lit Lit
  | Lam [Pttrn {- Arguments -}] (Maybe FITy {- Return -}) FITm {- Body -}
  | App FITm [FITm]
  | Let Pttrn FITm FITm
  | Cond FITm {- Pred -} FITm {- Then -} FITm {- Else -}
  | Tuple [FITm]
  | Proj FITm Label
  | Ann FITm FITy
  | Seq [FITm]
  | Rcd [(Label, FITm)]
  deriving (Show)

data Ty
  = -- Explicit types
    TyVar Name
  | TyPrim Prim
  | TyArrow [FITy] FITy
  | TyTuple [FITy]
  | TyRcd [(Label, FITy)]
  | TyApp FITy [FITy]
  | -- Generated types
    TyCast FITy FITy
  | TySeq [FITy]
  deriving (Show)

-- Other types
type Name = String

type Label = String

data Lit
  = LitNum Int
  | LitBool Bool
  | LitStr String
  | LitUnit
  deriving (Show)

data Pttrn
  = PttrnAtom Name
  | PttrnAnn Pttrn Ty
  | PttrnTuple [Pttrn]
  deriving (Show)

type FITm = FI Tm

type FITy = FI Ty

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Raw where

import qualified Tm

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
  | Lam [Pttrn {- Arguments -}] (Maybe Ty {- Return -}) Tm {- Body -}
  | App Tm [Tm]
  | Let Pttrn Tm Tm
  | Cond Tm {- Pred -} Tm {- Then -} Tm {- Else -}
  | Tuple [Tm]
  | Proj Tm Label
  | Ann Tm Ty
  | Seq [Tm]
  | Rcd [(Label, Tm)]
  deriving (Show)

data Ty
  = -- Explicit types
    TyVar Name
  | TyPrim Tm.Prim
  | TyArrow [Ty] Ty
  | TyTuple [Ty]
  | TyRcd [(Label, Ty)]
  | TyApp Ty [Ty]
  | -- Generated types
    TyCast Ty Ty
  | TySeq [Ty]
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
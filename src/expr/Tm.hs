module Tm where

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
  | TySeq [Ty]
  deriving (Show)

data Prim
  = PrimNum
  | PrimBool
  | PrimStr
  | PrimUnit
  deriving (Show)

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
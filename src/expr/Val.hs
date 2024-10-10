module Val where

import Raw (Name)
import Tm (Prim)
import qualified Tm as T (Ty (..))

type Env = [(Name, Ty)]

newtype Closure = Closure (Env, T.Ty)

data Ty
  = TyVar Name
  | TyPrim Prim
  | TyArrow Ty Ty
  | TyTuple [Ty]
  | TyRcd [(Name, Ty)]
  | TyLam Name Closure
module Val where

import Raw (Name)
import Tm (Prim)
import qualified Tm as T (Ty (..))

type Border = (Ty, Ty)

data Env = Env {types :: [Ty], borders :: [Border]}

data Closure = Closure Env T.Ty

data Ty
  = TyVar Int
  | TyPrim Prim
  | TyArrow [Ty] Ty
  | TyTuple [Ty]
  | TyRcd [(Name, Ty)]
  | TyLam Int Closure
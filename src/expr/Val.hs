module Val where

import Data.List (intercalate)
import Raw (Name)
import Tm (Prim)
import qualified Tm as T (Ty (..))
import Utils (PrettyShow (prettyShow), genTVarName)

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

instance PrettyShow Ty where
  prettyShow (TyVar i) = genTVarName i
  prettyShow (TyPrim p) = prettyShow p
  prettyShow (TyArrow tys ty) = "(" ++ intercalate ", " (map prettyShow tys) ++ ") -> " ++ prettyShow ty
  prettyShow (TyTuple tys) = "(" ++ intercalate ", " (map prettyShow tys) ++ ")"
  prettyShow (TyRcd tys) = "{" ++ intercalate ", " (map (\(l, t) -> l ++ ": " ++ prettyShow t) tys) ++ "}"
  prettyShow (TyLam i (Closure _ tms)) = "Forall(" ++ show i ++ ")" ++ "." ++ "<" ++ prettyShow tms ++ ">"

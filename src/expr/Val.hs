module Val where

import Data.List (intercalate)
import Raw (Name)
import Tm (Constr, Prim)
import qualified Tm as T (Ty (..))
import Utils (EvalState, PrettyShow (prettyShow), genTVarName)

type Border = (Ty, Ty)

type ConstrState = EvalState Tm.Constr Border

data Env = Env {types :: [Ty], constrs :: [ConstrState]}

data Closure = Closure Env T.Ty

data Ty
  = TyVar Int
  | TyPrim Prim
  | TyArrow [Ty] Ty
  | TyTuple [Ty]
  | TyRcd [(Name, Ty)]
  | TyLam Int Closure
  | TyTop
  | TyBot

instance PrettyShow Ty where
  prettyShow (TyVar i) = genTVarName i
  prettyShow (TyPrim p) = prettyShow p
  prettyShow (TyArrow tys ty) = "(" ++ intercalate ", " (map prettyShow tys) ++ ") -> " ++ prettyShow ty
  prettyShow (TyTuple tys) = "(" ++ intercalate ", " (map prettyShow tys) ++ ")"
  prettyShow (TyRcd tys) = "{" ++ intercalate ", " (map (\(l, t) -> l ++ ": " ++ prettyShow t) tys) ++ "}"
  prettyShow (TyLam i (Closure _ tms)) = "Forall(" ++ show i ++ ")" ++ "." ++ "<" ++ prettyShow tms ++ ">"
  prettyShow TyTop = "Top"
  prettyShow TyBot = "Bot"

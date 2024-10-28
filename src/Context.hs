{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Context where

import Raw (Def (..), Prog (..))
import qualified Raw as R
import qualified Tm as T
import Utils

data Expr
  = TyExpr (EvalState (FI R.Ty) (FI T.Ty))
  | TmExpr (EvalState (FI R.Tm) (FI T.Ty))
  | TmExpr' (EvalState (FI R.Ty) (FI T.Ty))
  deriving (Show)

type DumpedProgram = [(String, Expr)]

dumpProgram :: Prog -> DumpedProgram
dumpProgram (Prog (def : rest)) =
  let FI p d = def
      fi = FI p
   in case d of
        FuncDef n ps ret body ->
          (n, TmExpr . Uninterpreted . fi $ R.Lam ps ret body) : dumpProgram (Prog rest)
        ValDef n tm ->
          (n, TmExpr $ Uninterpreted tm) : dumpProgram (Prog rest)
        TyLet n ty ->
          (n, TyExpr $ Uninterpreted ty) : dumpProgram (Prog rest)
        EnumDef n polys flds -> flip (++) (dumpProgram (Prog rest)) $ do
          (fldName, tys') <- flds
          case tys' of
            [] -> do
              let ty = R.TyLam polys (fi $ R.TyPrim $ T.LitUDT n)
              return (fldName, TmExpr' . Uninterpreted . fi $ ty)
            tys -> do
              let ty = R.TyLam polys (fi $ R.TyArrow tys (fi $ R.TyPrim $ T.LitUDT n))
              return (fldName, TmExpr' . Uninterpreted . fi $ ty)
dumpProgram (Prog []) = []

getTestEntrance :: DumpedProgram -> FI R.Tm
getTestEntrance prog = case lookup "test" prog of
  Just (TmExpr (Uninterpreted tm)) -> tm
  _ -> error "No test entrance found"
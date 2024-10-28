{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module TC where

import Context
import Control.Arrow (Arrow (second), returnA, (>>^), (^>>))
import qualified Raw as R (Lit (..), Pttrn (..), Tm (..), Ty (..))
import TCUtils
import qualified Tm as T (Constr (Constr, bots, tops), Prim (..), Ty (..))
import Utils
import Prelude hiding ((<$))

infer :: FI R.Tm ->> FI T.Ty
infer = proc fitm -> do
  let FI p tm = fitm
  let fi = FI p
  case tm of
    R.Var x -> var -< fi x
    R.Lit lit -> case lit of
      R.LitNum _ -> returnA -< fi $ T.TyPrim T.PrimNum
      R.LitBool _ -> returnA -< fi $ T.TyPrim T.PrimBool
      R.LitStr _ -> returnA -< fi $ T.TyPrim T.PrimStr
      R.LitUnit -> returnA -< fi $ T.TyPrim T.PrimUnit
    R.Lam ps ret body -> do
      l0 <- () >- getEnv >>^ length . tvars
      tys <- ps >- fmapA inferPattern
      ret' <- infer -< body
      l1 <- () >- getEnv >>^ length . tvars
      let abs
            | l1 /= l0 = T.TyLam (l1 - l0) . fi
            | otherwise = id
      case ret of
        Just retT -> do
          retT <- toTmTy -< retT
          returnA -< fi $ abs $ T.TyArrow tys (fi (T.TyCast ret' retT))
        Nothing -> returnA -< fi $ abs $ T.TyArrow tys ret'
    R.App f args -> do
      FI p' fTy <- infer -< f
      argTys <- args >- fmapA infer
      case fTy of
        T.TyArrow tys ret
          | length argTys /= length tys -> p >- throwWith IncorrectParameterCount
          | otherwise -> do
              (argTys, tys) >- uncurry zip ^>> fmapA' unify
              let casts = uncurry T.TyCast <$> zip argTys tys
              returnA -< fi $ T.TySeq $ (fi <$> casts) ++ [ret]
        T.TyLam n (FI _ (T.TyArrow tys ret))
          | length argTys /= length tys -> p >- throwWith IncorrectParameterCount
          | otherwise -> do
              (argTys, tys) >- uncurry zip ^>> fmapA' unify
              env <- () >- getEnv
              setEnv -< tr env
              polyTys <- fmapA matchHoleTypes -< (zip tys argTys)
              let casts = uncurry T.TyCast <$> zip argTys tys
              let s = fi $ T.TySeq $ (fi <$> casts) ++ [ret]
              returnA -< fi $ T.TyApp (fi $ T.TyLam n s) (concat (tr' "PolyTys" polyTys))
        _ -> p' >- throwWith UndefinedBehavior
    R.Ann tm ty -> do
      ty1 <- toTmTy -< ty -- into
      ty2 <- infer -< tm -- from
      unify -< (ty2, ty1)
      env <- () >- getEnv
      setEnv -< tr env
      returnA -< fi $ T.TyCast ty2 ty1
    R.Tuple tms -> do
      t <- tms >- fmapA infer >>^ T.TyTuple
      returnA -< fi t
    R.Rcd rcd -> do
      indices <- fmapA (fst ^>> getRecordIndex) -< rcd
      t <- (rcd !!) <$> indices >- fmapA (second infer) >>^ T.TyRcd
      returnA -< fi t
    R.Proj tm l -> do
      FI _ rcdTy <- infer -< tm
      case rcdTy of
        T.TyRcd rcd -> case lookup l rcd of
          Just ty -> returnA -< ty
          Nothing -> p >- throwWith UndefinedField
        T.TyLam n (FI _ (T.TyRcd rcd)) -> case lookup l rcd of
          Just ty -> returnA -< fi $ T.TyLam n ty
          Nothing -> p >- throwWith UndefinedField
        T.TyVar _ -> do
          newTVar -< "%T." ++ l
          ty <- () >- getEnv >>^ T.TyVar . subtract 1 . length . tvars
          unify -< (fi rcdTy, fi $ T.TyRcd [(l, fi ty)])
          returnA -< fi ty
        _ -> p >- throwWith NotARecord
    R.Cond cnd thn els -> do
      cndTy <- infer -< cnd
      thnTy <- infer -< thn
      elsTy <- infer -< els
      let fi' = FI (pos $ tr cndTy)
      unify -< (cndTy, fi' $ T.TyPrim T.PrimBool)
      unify -< (thnTy, elsTy)
      let cast = fi' $ T.TyCast cndTy (fi' $ T.TyPrim T.PrimBool)
      returnA -< fi $ T.TySeq [cast, fi $ T.TyBiCast thnTy elsTy]
    R.Seq tms -> do
      t <- tms >- fmapA infer >>^ T.TySeq
      returnA -< fi t

    -- Macros
    R.Macro x tm -> do
      ty <- infer -< tm
      returnA -< fi $ T.TyMacro x ty
    _ -> p >- throwWith UndefinedBehavior

unify :: (FI T.Ty, FI T.Ty) ->> ()
unify = proc t -> do
  let (FI _ ty1, FI p2 ty2) = t
  case (ty1, ty2) of
    (T.TyVar i, T.TyVar j) -> do
      ci <- getTConstr -< i
      cj <- getTConstr -< j
      let bots = T.bots cj ++ T.bots ci
      setTConstr -< (i, T.Constr bots (T.tops ci))
    (T.TyVar i, ty) -> addBot -< (i, FI p2 ty)
    -- (ty, T.TyVar i) -> addTop -< (i, FI p1 ty)
    (T.TyTuple tys1, T.TyTuple tys2) ->
      (tys1, tys2) >- uncurry zip ^>> fmapA' unify
    (T.TyArrow tys1 ty1, T.TyArrow tys2 ty2) -> do
      (tys1, tys2) >- uncurry zip ^>> fmapA' unify
      unify -< (ty1, ty2)
    (T.TyRcd rcd1, T.TyRcd rcd2) ->
      zip rcd1 rcd2
        >-
          fmapA' $ proc ((l1, ty1), (l2, ty2)) -> do
            if l1 == l2
              then unify -< (ty1, ty2)
              else returnA -< ()
    (T.TyApp ty1 tys1, T.TyApp ty2 tys2) -> do
      unify -< (ty1, ty2)
      (tys1, tys2) >- uncurry zip ^>> fmapA' unify
    (T.TyCast ty1 ty2, T.TyCast ty3 ty4) -> do
      unify -< (ty1, ty3)
      unify -< (ty2, ty4)
    (T.TySeq tys1, T.TySeq tys2) ->
      (tys1, tys2) >- uncurry zip ^>> fmapA' unify
    (T.TyLam _ ty1, T.TyLam _ ty2) -> do
      unify -< (ty1, ty2)
    _ -> returnA -< ()

inferPattern :: FI R.Pttrn ->> FI T.Ty
inferPattern = proc fip -> do
  let FI p' p = fip
  let fi = FI p'
  case p of
    R.PttrnAtom x -> do
      newTVar -< "%T" ++ x
      ty <- () >- getEnv >>^ T.TyVar . subtract 1 . length . tvars
      newVar -< (x, fi ty)
    R.PttrnTuple ps -> do
      t <- ps >- fmapA inferPattern >>^ T.TyTuple
      returnA -< fi t
    R.PttrnAnn p ty -> do
      ty <- toTmTy -< ty
      checkPattern -< (p, ty)
      returnA -< ty

checkPattern :: (FI R.Pttrn, FI T.Ty) ->> ()
checkPattern = proc t -> do
  let (FI p1 pttrn, FI p2 ty') = t
  case (pttrn, ty') of
    (R.PttrnAtom x, ty) -> (x, FI p2 ty) >- newVar >>^ const ()
    (R.PttrnTuple ps, T.TyTuple tys) ->
      (ps, tys) >- uncurry zip ^>> fmapA checkPattern >>^ const ()
    (R.PttrnAnn _ _, _) -> p1 >- throwWith MultipleAnnotation
    _ -> p1 >- throwWith UndefinedPattern

toTmTy :: FI R.Ty ->> FI T.Ty
toTmTy = proc fity -> do
  let FI p ty = fity
  let fi = FI p
  case ty of
    R.TyVar x -> tvar -< fi x
    R.TyPrim prim -> returnA -< fi $ T.TyPrim prim
    R.TyArrow tys ty -> do
      tys <- tys >- fmapA toTmTy
      ty <- ty >- toTmTy
      returnA -< fi $ T.TyArrow tys ty
    R.TyTuple tys -> do
      t <- tys >- fmapA toTmTy >>^ T.TyTuple
      returnA -< fi t
    R.TyRcd rcd -> do
      t <- rcd >- fmapA (second toTmTy) >>^ T.TyRcd
      returnA -< fi t
    R.TyApp ty tys -> do
      ty <- ty >- toTmTy
      tys <- tys >- fmapA toTmTy
      returnA -< fi $ T.TyApp ty tys
    R.TyCast ty1 ty2 -> do
      ty1 <- ty1 >- toTmTy
      ty2 <- ty2 >- toTmTy
      returnA -< fi $ T.TyCast ty1 ty2
    R.TySeq tys -> do
      t <- tys >- fmapA toTmTy >>^ T.TySeq
      returnA -< fi t
    R.TyLam ns ty -> do
      fmapA' newTVar -< tr ns
      ty <- ty >- toTmTy
      returnA -< fi $ T.TyLam (length ns) ty

var :: FI Name ->> FI T.Ty
var = proc x -> do
  env <- getEnv -< ()
  case lookup (val x) (vars env) of
    Just ty -> returnA -< ty
    Nothing -> case lookup (val x) (globalSyms env) of
      Just (TmExpr (Interpreted t)) -> returnA -< t
      Just (TmExpr (Uninterpreted t)) -> do
        ty <- infer -< t
        let mod = lookupAndReplace (val x) (TmExpr (Interpreted ty))
        modifyEnv -< \e -> e {globalSyms = mod (globalSyms e)}
        returnA -< ty
      Just (TmExpr' (Interpreted t)) -> returnA -< t
      Just (TmExpr' (Uninterpreted t)) -> do
        ty <- toTmTy -< t
        let mod = lookupAndReplace (val x) (TmExpr' (Interpreted ty))
        modifyEnv -< \e -> e {globalSyms = mod (globalSyms e)}
        returnA -< ty
      _ -> pos x >- throwWith UnboundVariable

tvar :: FI Name ->> FI T.Ty
tvar = proc x -> do
  env <- getEnv -< ()
  case lookupIndex (val x) (tvars env) of
    Just i -> returnA -< FI (pos x) $ T.TyVar i
    Nothing -> case lookup (val x) (globalSyms env) of
      Just (TyExpr (Interpreted ty)) -> returnA -< ty
      Just (TyExpr (Uninterpreted ty)) -> do
        ty <- toTmTy -< ty
        let mod = lookupAndReplace (val x) (TyExpr (Interpreted ty))
        modifyEnv -< \e -> e {globalSyms = mod (globalSyms e)}
        returnA -< ty
      _ -> pos x >- throwWith UnboundTypeVariable

matchHoleTypes :: (FI T.Ty, FI T.Ty) ->> [FI T.Ty]
matchHoleTypes = proc t -> do
  let (FI _ ty1, FI p2 ty2) = t
  case (ty1, ty2) of
    (T.TyVar i, ty) -> do
      let hd = [FI p2 ty]
      T.Constr {T.bots = bs} <- getTConstr -< i
      tys <- map (,FI p2 ty) (tr' "BOts" bs) >- fmapA matchHoleTypes >>^ concat
      returnA -< hd ++ tys
    (T.TyApp ty1 tys1, T.TyApp ty2 tys2) -> do
      tys <- matchHoleTypes -< (ty1, ty2)
      tys' <- fmapA matchHoleTypes >>^ concat -< (zip tys1 tys2)
      returnA -< tys' ++ tys
    (T.TyArrow tys1 ty1, T.TyArrow tys2 ty2) -> do
      tys <- matchHoleTypes -< (ty1, ty2)
      tys' <- fmapA matchHoleTypes >>^ concat -< (zip tys1 tys2)
      returnA -< tys' ++ tys
    (T.TyCast ty1 ty2, T.TyCast ty3 ty4) -> do
      tys1 <- matchHoleTypes -< (ty1, ty3)
      tys2 <- matchHoleTypes -< (ty2, ty4)
      returnA -< tys1 ++ tys2
    (T.TyTuple tys1, T.TyTuple tys2) -> do
      tys <- fmapA matchHoleTypes -< zip tys1 tys2
      returnA -< concat tys
    (T.TyRcd rcd1, T.TyRcd rcd2) -> do
      tys <- fmapA matchHoleTypes -< getRecordIntersection rcd1 rcd2
      returnA -< concat tys
    (T.TySeq tys1, T.TySeq tys2) -> do
      tys <- fmapA matchHoleTypes -< zip tys1 tys2
      returnA -< concat tys
    _ -> returnA -< []
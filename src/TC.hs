{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module TC where

import Control.Arrow (Arrow (second), returnA, (>>^), (^>>))
import Data.List (findIndex, isSubsequenceOf, sortOn)
import qualified Raw as R (Label, Lit (..), Pttrn (..), Tm (..), Ty (..))
import qualified Tm as T (Constr (Constr, bots, tops), Label, Prim (..), Ty (..))
import Utils
import Prelude hiding ((<$))

infer :: R.Tm ->> T.Ty
infer = proc tm -> case tm of
  R.Var x -> var -< x
  R.Lit lit -> case lit of
    R.LitNum _ -> returnA -< T.TyPrim T.PrimNum
    R.LitBool _ -> returnA -< T.TyPrim T.PrimBool
    R.LitStr _ -> returnA -< T.TyPrim T.PrimStr
    R.LitUnit -> returnA -< T.TyPrim T.PrimUnit
  R.Lam ps ret body -> do
    l0 <- () >- getEnv >>^ length . vars
    tys <- ps >- fmapA inferPattern
    ret' <- infer -< body
    l1 <- () >- getEnv >>^ length . vars
    let abs = T.TyLam (l1 - l0)
    case ret of
      Just retT -> do
        retT <- toTmTy -< retT
        returnA -< abs $ T.TyArrow tys (T.TyCast ret' retT)
      Nothing -> returnA -< abs $ T.TyArrow tys ret'
  R.App f args -> do
    fTy <- infer -< f
    argTys <- args >- fmapA infer
    case fTy of
      T.TyLam n (T.TyArrow tys ret)
        | length argTys /= length tys -> () >- throw IncorrectParameterCount
        | otherwise -> do
            (argTys, tys) >- uncurry zip ^>> fmapA' unify
            let s = T.TySeq $ (T.TyCast <$> tys <*> argTys) ++ [ret]
            returnA -< T.TyApp (T.TyLam n s) argTys
      _ -> () >- throw UndefinedBehavior
  R.Ann tm ty -> do
    ty1 <- toTmTy -< ty -- into
    ty2 <- infer -< tm -- from
    unify -< (ty2, ty1)
    returnA -< T.TyCast ty2 ty1
  R.Tuple tms -> tms >- fmapA infer >>^ T.TyTuple
  R.Rcd rcd -> sortOn fst rcd >- fmapA (second infer) >>^ T.TyRcd
  R.Proj tm l -> do
    rcdTy <- infer -< tm
    case rcdTy of
      T.TyRcd rcd -> case lookup l rcd of
        Just ty -> returnA -< ty
        Nothing -> () >- throw UndefinedField
      T.TyLam n (T.TyRcd rcd) -> case lookup l rcd of
        Just ty -> returnA -< T.TyLam n ty
        Nothing -> () >- throw UndefinedField
      _ -> () >- throw NotARecord
  R.Cond cnd thn els -> do
    cndTy <- infer -< cnd
    thnTy <- infer -< thn
    elsTy <- infer -< els
    unify -< (cndTy, T.TyPrim T.PrimBool)
    unify -< (thnTy, elsTy)
    unify -< (elsTy, thnTy)
    returnA -< T.TySeq [T.TyCast cndTy (T.TyPrim T.PrimBool), T.TyBiCast thnTy elsTy]
  R.Seq tms -> tms >- fmapA infer >>^ T.TySeq
  _ -> () >- throw UndefinedBehavior

unify :: (T.Ty, T.Ty) ->> ()
unify = proc t -> case t of
  (T.TyVar i, T.TyVar j) -> do
    ci <- getTConstr -< i
    cj <- getTConstr -< j
    let bots = T.bots cj ++ T.bots ci
    let tops = T.tops ci ++ T.tops cj
    setTConstr -< (i, T.Constr bots tops)
  (T.TyVar i, ty) -> addBot -< (i, ty)
  (ty, T.TyVar i) -> addTop -< (i, ty)
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

inferPattern :: R.Pttrn ->> T.Ty
inferPattern = proc p -> case p of
  R.PttrnAtom x -> do
    newTVar -< "%T" ++ x
    ty <- () >- getEnv >>^ T.TyVar . length . tvars
    newVar -< (x, ty)
  R.PttrnTuple ps ->
    ps >- fmapA inferPattern >>^ T.TyTuple
  R.PttrnAnn p ty -> do
    ty <- toTmTy -< ty
    checkPattern -< (p, ty)
    returnA -< ty

checkPattern :: (R.Pttrn, T.Ty) ->> ()
checkPattern = proc t -> case t of
  (R.PttrnAtom x, ty) -> (x, ty) >- newVar >>^ const ()
  (R.PttrnTuple ps, T.TyTuple tys) ->
    (ps, tys) >- uncurry zip ^>> fmapA checkPattern >>^ const ()
  (R.PttrnAnn _ _, _) -> () >- throw MultipleAnnotation
  _ -> () >- throw UndefinedPattern

toTmTy :: R.Ty ->> T.Ty
toTmTy = proc ty -> case ty of
  R.TyVar x -> do
    vs <- () >- getEnv >>^ vars
    case findIndex ((== x) . fst) vs of
      Just i -> returnA -< T.TyVar i
      Nothing -> () >- throw UnboundVariable
  R.TyPrim prim -> returnA -< T.TyPrim prim
  R.TyArrow tys ty -> do
    tys <- tys >- fmapA toTmTy
    ty <- ty >- toTmTy
    returnA -< T.TyArrow tys ty
  R.TyTuple tys -> tys >- fmapA toTmTy >>^ T.TyTuple
  R.TyRcd rcd -> sortOn fst rcd >- fmapA (second toTmTy) >>^ T.TyRcd
  R.TyApp ty tys -> do
    ty <- ty >- toTmTy
    tys <- tys >- fmapA toTmTy
    returnA -< T.TyApp ty tys
  R.TyCast ty1 ty2 -> do
    ty1 <- ty1 >- toTmTy
    ty2 <- ty2 >- toTmTy
    returnA -< T.TyCast ty1 ty2
  R.TySeq tys -> tys >- fmapA toTmTy >>^ T.TySeq
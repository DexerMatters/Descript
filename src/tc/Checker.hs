{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{-# LANGUAGE TupleSections #-}

module Checker where

import           Control.Monad (zipWithM_)
import           Control.Monad.Error.Class (MonadError(throwError))
import           Control.Monad.RWS (gets, MonadState(get))
import           Pattern
import qualified Raw as R
import           State
import           Tm as T (Ctx(tvars), FITy
                        , Prim(PrimUnit, PrimNum, PrimBool, PrimStr)
                        , Ty(TyRcd, TyPrim, TyReduce, TyLam, TyCast, TyVar, TyApp, TyArrow,
   TyTuple, TyBiCast)
                        , emptyConstr)
import           Utils
import           Dbg (traceInfo, printM)

infer :: R.FITm ->> T.FITy
infer = \case
  p :| R.Var x -> inferVar (p :| x)
  p :| R.Tuple ts -> FI p . T.TyTuple <$> mapM infer ts
  p :| R.Rcd flds -> let inferFld (l, t) = (l, ) <$> infer t
                     in FI p . T.TyRcd <$> mapM inferFld flds
  p :| R.Lit l -> pure
    $ p
    :| case l of
      R.LitNum _  -> T.TyPrim T.PrimNum
      R.LitBool _ -> T.TyPrim T.PrimBool
      R.LitStr _  -> T.TyPrim T.PrimStr
      R.LitUnit   -> T.TyPrim T.PrimUnit
  p :| R.Lam ps ret body -> do
    -- Introduce variable and possible type variable from given argument patterns
    -- l0 and l are for how many type arguments the function holds
    l0 <- gets (length . tvars)
    argTys <- mapM inferPattern ps
    bodyTy <- infer body
    retTy <- case ret of
      Just ty -> FI p . T.TyCast bodyTy <$> indexType ty
      Nothing -> pure bodyTy
    l <- gets (length . tvars)
    -- Lock all type variables introduced by the argument patterns and the body
    -- so that they won't be unified in other scopes
    mapM_ lockConstr [l0 .. l - 1]
    -- If there is no assigned type variable then it's unnecessary to employ TyLam
    let f
          | l /= l0 = FI p . T.TyLam (l - l0)
          | otherwise = id
    return $ f $ p :| T.TyArrow argTys retTy
  p :| R.App lam args -> do
    argTys <- mapM infer args
    lamTy <- infer lam
    case lamTy of
      _ :| T.TyArrow tys _ -> do
        zipWithM_ unify tys argTys
        return $ p :| T.TyReduce lamTy argTys
      _ :| T.TyLam _ (_ :| T.TyArrow tys _) -> do
        zipWithM_ unify tys argTys
        return $ p :| T.TyReduce lamTy argTys
      -- Neutrals
      _ -> return $ p :| T.TyReduce lamTy argTys
  p :| R.Proj tar l -> do
    tarTy <- infer tar
    case tarTy of
      _ :| T.TyRcd flds -> case lookup l flds of
        Just ty -> pure ty
        Nothing -> throwError $ MissingLabel (p :| l)
      -- Projection will reserve type abstractions of the target
      p' :| T.TyLam n (_ :| T.TyRcd flds) -> case lookup l flds of
        Just ty -> pure $ p' :| T.TyLam n ty
        Nothing -> throwError $ MissingLabel (p :| l)
      -- Projection will unify the target with a type variable if possible
      _ :| T.TyVar i -> do
        tvar <- newTVar ("%T/proj" ++ l) T.emptyConstr
        addBot i (T.TyRcd [(l, p :| tvar)])
        return $ p :| tvar
      _ -> throwError $ NonProjectableType tarTy
  p :| R.Ann tm ty -> do
    ty' <- indexType ty
    tmTy <- infer tm
    tvs <- get
    printM $ show tvs
    unify tmTy ty'
    return $ p :| T.TyCast tmTy ty'
  _ :| R.Seq tms -> do
    tys <- mapM infer tms
    return $ last tys
  p :| R.Cond cnd thn els -> do
    cndTy <- infer cnd
    thnTy <- infer thn
    elsTy <- infer els
    unify cndTy (p :| T.TyPrim T.PrimBool)
    unify thnTy elsTy
    return $ p :| T.TyBiCast thnTy elsTy
  ty -> error $ "impossible :" ++ show ty

unify :: T.FITy -> T.FITy ->> ()
unify = curry
  $ \case
    T.TyVar i :<*>: ty -> addBot i (traceInfo ty)
    T.TyApp ty tys :<*>: T.TyApp ty' tys' -> do
      unify ty ty'
      zipWithM_ unify tys tys'
    T.TyArrow tys ty :<*>: T.TyArrow tys' ty' -> do
      zipWithM_ unify tys tys'
      unify ty ty'
    T.TyTuple tys :<*>: T.TyTuple tys' -> zipWithM_ unify tys tys'
    T.TyRcd tys :<*>: T.TyRcd tys' -> do
      let sames = [(t, t') | (l, t) <- tys, (l', t') <- tys', l == l']
      mapM_ (uncurry unify) sames
    _ -> pure ()

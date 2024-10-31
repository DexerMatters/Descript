{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{-# LANGUAGE TupleSections #-}

module Checker where

import           Control.Monad (zipWithM_)
import           Control.Monad.Error.Class (MonadError(throwError))
import           Control.Monad.RWS (gets)
import           Data.Bool (bool)
import           Pattern
import qualified Raw as R
import           State
import           Tm as T
import           Utils

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
    -- If there is no assigned type variable then it's unnecessary to employ TyLam
    let f
          | l /= l0 = FI p . T.TyLam (l - l0)
          | otherwise = id
    return $ f $ p :| T.TyArrow argTys retTy
  p :| R.App lam args -> do
    lamTy <- infer lam
    argTys <- mapM infer args
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
  p :| R.Ann tm ty -> do
    ty' <- indexType ty
    tmTy <- infer tm
    unify ty' tmTy
    return $ p :| T.TyCast tmTy ty'
  _ -> error "impossible"

unify :: T.FITy -> T.FITy ->> ()
unify = curry
  $ \case
    _ :| T.TyVar i :<>: _ :| ty -> addBot i ty
    _ :| T.TyApp ty tys :<>: _ :| T.TyApp ty' tys' -> do
      unify ty ty'
      zipWithM_ unify tys tys'
    _ :| T.TyArrow tys ty :<>: _ :| T.TyArrow tys' ty' -> do
      zipWithM_ unify tys tys'
      unify ty ty'
    _ :| T.TyTuple tys :<>: _ :| T.TyTuple tys' -> zipWithM_ unify tys tys'
    _ :| T.TyRcd tys :<>: _ :| T.TyRcd tys' -> do
      let sames = [(t, t') | (l, t) <- tys, (l', t') <- tys', l == l']
      mapM_ (uncurry unify) sames
    _ -> pure ()

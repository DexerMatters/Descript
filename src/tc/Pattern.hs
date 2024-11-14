{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}

module Pattern where

import           Control.Monad (zipWithM)
import           Control.Monad.Error.Class (MonadError(throwError))
import           Control.Monad.State.Lazy (get)
import           Data.Functor (($>))
import           Raw as R
import           State
import           Tm as T
import           Utils
import           Prelude hiding (lookup)

inferPattern :: FI R.Pttrn ->> FI T.Ty
inferPattern = \case
  p :| R.PttrnAtom x -> newTVar x T.emptyConstr >>= newVar x >>= pure . FI p
  _ :| R.PttrnAnn pttrn ty -> indexType ty >>= checkPattern pttrn
  p :| R.PttrnTuple ps -> mapM inferPattern ps >>= pure . FI p . T.TyTuple
  FI _ _ -> error "impossible"

checkPattern :: FI R.Pttrn -> FI T.Ty ->> FI T.Ty
checkPattern = curry
  $ \case
    R.PttrnAtom x :<*: p :| ty -> FI p <$> newVar x ty
    c@(R.PttrnAnn _ _ :<*: _) -> throwError $ uncurry BadPattern c
    R.PttrnTuple ps :<*: p
      :| T.TyTuple tys -> zipWithM checkPattern ps tys $> p :| T.TyTuple tys
    c -> throwError $ uncurry BadPattern c

indexType :: FI R.Ty ->> FI T.Ty
indexType = \case
  p :| R.TyVar x -> do
    Ctx { tvars, globalTypes } <- get
    case x `lookupIndex` tvars of
      Just i  -> pure $ p :| T.TyVar i
      Nothing -> case x `lookup` globalTypes of
        Just ty -> pure $ p :| ty
        Nothing -> throwError $ UnboundType $ p :| x
  p :| R.TyPrim prim -> pure $ p :| T.TyPrim prim
  p :| R.TyArrow tys ty -> do
    tys' <- mapM indexType tys
    ty' <- indexType ty
    pure $ p :| T.TyArrow tys' ty'
  p :| R.TyTuple tys -> do
    tys' <- mapM indexType tys
    pure $ p :| T.TyTuple tys'
  p :| R.TyRcd tys -> do
    tys' <- mapM (\(l, ty) -> (l, ) <$> indexType ty) tys
    pure $ p :| T.TyRcd tys'
  p :| R.TyLam names body -> do
    mapM_ (`newTVar` T.emptyConstr) names
    body' <- indexType body
    pure $ p :| T.TyLam (length names) body'
  p :| R.TyApp ty tys -> do
    ty' <- indexType ty
    tys' <- mapM indexType tys
    pure $ p :| T.TyApp ty' tys'
  p :| R.TyCast ty1 ty2 -> do
    ty1' <- indexType ty1
    ty2' <- indexType ty2
    pure $ p :| T.TyCast ty1' ty2'
  FI _ _ -> error "impossible"

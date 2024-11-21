{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module TypeLift where

import qualified Raw as R
import           State (TmState, isolateWith)
import qualified Tm as T
import           Utils
import           Control.Monad.RWS (gets)
import           Data.Sequence (fromList)
import           Tm (emptyCtx)

liftType :: R.Ty -> TmState T.Ty
liftType = \case
  R.TyVar x        -> do
    ty <- gets ((!!? x) . R.types . T.symbols)
    s <- gets T.symbols
    case ty of
      Just raw -> isolateWith (emptyCtx s) (liftType raw)
      Nothing  -> error "not yet implemented"
  R.TyPrim p       -> pure $ T.TyPrim p
  R.TyArrow tys ty -> T.TyArrow <$> mapM liftType tys <*> liftType ty
  R.TyTuple tys    -> T.TyTuple <$> mapM liftType tys
  R.TyRcd tys      -> T.TyRcd <$> mapM (secondM liftType) tys
  R.TyApp ty tys   -> T.TyApp <$> liftType ty <*> mapM liftType tys <*> pure []
  R.TyCast ty ty'  -> T.TyCast <$> liftType ty <*> liftType ty'
  R.TyLam _ _      -> error "not yet implemented"

liftPattern :: R.Pttrn -> TmState T.Pttrn
liftPattern = \case
  R.PttrnAtom x   -> pure $ T.PttrnAtom x
  R.PttrnTuple ps -> T.PttrnTuple <$> mapM liftPattern ps
  R.PttrnAnn p ty -> T.PttrnAnn <$> liftPattern p <*> liftType ty

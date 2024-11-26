{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}

module Pattern where

import           Control.Monad (void, zipWithM_)
import           Control.Monad.Error.Class (MonadError(throwError))
import           State (newTyVar, putVar, TmState)
import           Tm (Pttrn(..), Ty(..))
import           Prelude hiding (lookup)
import           Errors (RTError(..))

inferFromPattern :: Pttrn -> TmState Ty
inferFromPattern = \case
  PttrnAtom x   -> do
    ty <- uncurry TyVar <$> newTyVar
    void $ putVar x ty
    return ty
  PttrnTuple ps -> do
    tys <- mapM inferFromPattern ps
    return $ TyTuple tys
  PttrnAnn p ty -> checkFromPattern p ty >> return ty

checkFromPattern :: Pttrn -> Ty -> TmState ()
checkFromPattern = curry
  $ \case
    (PttrnAtom x, ty) -> void $ putVar x ty
    (PttrnTuple ps, TyTuple tys) -> zipWithM_ checkFromPattern ps tys
    (p, ty) -> throwError $ BadPattern p ty

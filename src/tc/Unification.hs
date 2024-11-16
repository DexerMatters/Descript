{-# LANGUAGE LambdaCase #-}

module Unification where

import           Control.Monad (zipWithM_)
import           State
import           Tm (Ty(..))
import           Utils (Constraint(Bot, Top))

unify :: Ty -> Ty -> TmState ()
unify = curry
  $ \case
    (TyVar i, ty) -> restrict (Bot ty) i
    (ty, TyVar i) -> restrict (Top ty) i
    ( TyApp ty tys _
      , TyApp ty' tys' _) -> unify ty ty' >> zipWithM_ unify tys tys'
    (TyTuple tys, TyTuple tys') -> zipWithM_ unify tys tys'
    (TyRcd tys, TyRcd tys')
      -> sequence_ [unify ty ty' | (l, ty) <- tys, (l', ty') <- tys', l == l']
    ( TyArrow tys ty
      , TyArrow tys' ty') -> zipWithM_ unify tys tys' >> unify ty ty'
    _ -> pure ()

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Checker where

import           Pattern
import qualified Raw as R
import           State
import           Tm as T
import           Utils
import           Control.Monad (zipWithM_)
import           Data.Map (unionWith)
import           Data.List (intersectBy)

infer :: R.FITm ->> T.FITy
infer = \case
  p :| R.Var x -> inferVar (p :| x)
  p :| R.Lit l -> pure
    $ p
    :| case l of
      R.LitNum _  -> T.TyPrim T.PrimNum
      R.LitBool _ -> T.TyPrim T.PrimBool
      R.LitStr _  -> T.TyPrim T.PrimStr
      R.LitUnit   -> T.TyPrim T.PrimUnit

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
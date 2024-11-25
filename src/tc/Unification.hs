{-# LANGUAGE LambdaCase #-}

module Unification where

import           Control.Monad (zipWithM_)
import           State
import           Tm (Ty(..), Ctx(constrs), Constr(elems))
import           Utils (Constraint(Bot, Top), liftConstraint)
import           Control.Monad.State (gets)
import           Data.Maybe (fromJust)
import           Prelude hiding (lookup)
import           Data.Sequence (lookup, Seq, fromList)
import           Dbg (printM)
import           Data.Traversable (for)
import           Control.Monad.Error.Class (MonadError(throwError))
import           Errors (RTError(NonApplicableType))
import           Data.List (transpose)
import           Data.Functor ((<&>))
import qualified Tm as V
import qualified Data.List as Data.Sequence
import           Data.Foldable (Foldable(toList))

unify :: Ty -> Ty -> TmState ()
unify = curry
  $ \case
    (TyVar i _, TyVar j _) -> do
      constrs' <- gets (fromJust . lookup j . constrs)
      mapM_ (`restrict` i) (elems constrs')
    (TyVar i _, ty) -> restrict (Bot ty) i
    (ty, TyVar i _) -> restrict (Top ty) i
    ( TyApp ty tys _
      , TyApp ty' tys' _) -> unify ty ty' >> zipWithM_ unify tys tys'
    (TyTuple tys, TyTuple tys') -> zipWithM_ unify tys tys'
    (TyRcd tys, TyRcd tys')
      -> sequence_ [unify ty ty' | (l, ty) <- tys, (l', ty') <- tys', l == l']
    ( TyArrow tys ty
      , TyArrow tys' ty') -> zipWithM_ unify tys tys' >> unify ty ty'
    _ -> pure ()

collectArgConstrs :: Int -> TmState [[Constraint Ty]]
collectArgConstrs i = do
  constrs' <- gets (elems . fromJust . lookup i . constrs)
  mapM
    liftConstraint
    (flip fmap (toList constrs')
     $ fmap
     $ \case
       TyArrow tys _ -> pure tys
       TyLam _ _ (TyArrow tys _) -> pure tys
       ty -> throwError $ NonApplicableType ty)
    <&> transpose . fmap liftConstraint

collectRetConstrs :: Int -> TmState [Constraint Ty]
collectRetConstrs i = do
  constrs' <- gets (elems . fromJust . lookup i . constrs)
  mapM liftConstraint
    $ flip fmap (toList constrs')
    $ fmap
    $ \case
      TyArrow _ ty -> pure ty
      TyLam _ _ (TyArrow _ ty) -> pure ty
      ty -> throwError $ NonApplicableType ty
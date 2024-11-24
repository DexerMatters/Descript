{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module TypeInfer where

import           Control.Monad (zipWithM_, (>=>))
import           Control.Monad.Error.Class (MonadError(throwError))
import           Control.Monad.State (gets)
import           Data.Functor ((<&>))
import           Pattern (inferFromPattern, checkFromPattern)
import           Prelude hiding (lookup)
import qualified Raw as R
import           State (TmState, lockConstr, newTyVar, restrict, isolateWith
                      , newTyVarWithConstr, updateLevel)
import qualified Tm as T
import           TypeLift (liftPattern, liftType)
import           Unification (unify, collectArgConstrs, collectRetConstrs)
import           Utils
import           Data.List (lookup)
import           Errors (RTError(NonProjectableType, UnboundVar, DissatisfiedParameterCount,
        MissingLabel))
import           Dbg (printM)
import qualified Data.Sequence as Sequence
import           Data.Maybe (fromJust)

infer :: R.Tm -> TmState T.Ty
infer = \case
  R.Var x           -> do
    ty <- gets ((!!?~ x) . T.vars)
    case ty of
      Just ty' -> pure ty'
      Nothing  -> do
        tm' <- gets ((!!?~ x) . R.terms . T.symbols)
        -- s <- gets T.symbols
        case tm' of
          Just tm -> infer tm
          Nothing -> throwError $ UnboundVar x
  R.Lit l           -> pure
    $ T.TyPrim
    $ case l of
      R.LitNum _  -> R.PrimNum
      R.LitBool _ -> R.PrimBool
      R.LitStr _  -> R.PrimStr
      R.LitUnit   -> R.PrimUnit
  R.Lam ps ret body -> do
    -- Infer types of the function and calculate the count of type arguments
    l0 <- gets (length . T.constrs)
    tys <- mapM (liftPattern >=> inferFromPattern) ps
    bodyT <- infer body
    l1 <- gets (length . T.constrs)
    updateLevel
    let count = l1 - l0
    -- Create the return type of the function
    retT <- maybe (pure bodyT) (liftType >=> pure . T.TyCast bodyT) ret
    -- Lock all constraints created after the inference of the function
    -- so that they are not affected by the other scopes
    mapM_ lockConstr [l0 .. l1 - 1]
    -- Consider whether to introduce a type lambda
    pure
      $ if count == 0
        then T.TyArrow tys retT
        else T.TyLam count $ T.TyArrow tys retT
  R.App f arg       -> do
    -- First arg of T.TyReduce is the evidence of deduction
    fT <- infer f
    argT <- mapM infer arg
    case fT of
      T.TyArrow argT' retT
        | length argT' == length argT -> zipWithM_ unify argT' argT
          >> pure (T.TyApp retT argT argT')
        | otherwise -> throwError $ DissatisfiedParameterCount (length argT)
      T.TyLam _ (T.TyArrow argT' retT)
        | length argT' == length argT -> zipWithM_ unify argT' argT
          >> pure (T.TyApp retT argT argT')
        | otherwise -> throwError $ DissatisfiedParameterCount (length argT)
      T.TyVar x _ -> do
        -- TODO: There is a serious issue with this code:
        -- Type variable that is introduced out of thin air won't be substituted
        -- by a corresponding type lambda
        constrs <- collectArgConstrs x
        constrs' <- collectRetConstrs x
        vars <- mapM newTyVarWithConstr constrs
        ret <- newTyVarWithConstr constrs'
        return $ T.TyApp (uncurry T.TyVar ret) argT (uncurry T.TyVar <$> vars)
      _ -> error "App"
  R.Ann tm ty       -> do
    ty' <- liftType ty
    tmT <- infer tm
    unify tmT ty'
    pure $ T.TyCast tmT ty'
  R.Tuple tms       -> T.TyTuple <$> mapM infer tms
  R.Rcd tms         -> T.TyRcd <$> mapM (secondM infer) tms
  R.Proj tm l       -> do
    tmT <- infer tm
    case tmT of
      T.TyRcd flds -> maybe (throwError $ MissingLabel l) pure $ lookup l flds
      T.TyLam _ (T.TyRcd flds) -> maybe (throwError $ MissingLabel l) pure
        $ lookup l flds
      T.TyVar i _ -> do
        tvar <- newTyVar <&> uncurry T.TyVar
        restrict (Bot $ T.TyRcd [(l, tvar)]) i
        return tvar
      _ -> throwError $ NonProjectableType tmT
  R.Cond c t f      -> do
    cT <- infer c
    tT <- infer t
    fT <- infer f
    unify cT (T.TyPrim R.PrimBool)
    unify fT tT
    pure $ T.TyBiCast tT fT
  R.Seq tms         -> do
    mapM_ infer (init tms)
    infer (last tms)
  R.Let p rhs body  -> do
    rhsT <- infer rhs
    liftPattern p >>= flip checkFromPattern rhsT
    updateLevel
    infer body
  R.Macro _ _       -> error "Macro types"
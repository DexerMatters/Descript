{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{-# LANGUAGE LambdaCase #-}

module TypeQuote where

import qualified Val as V
import           State (ValState, fresh)
import           Subtyping
import           Data.List (intercalate)
import           Utils (secondM, (!!!), Constraint(..))
import           Control.Monad.State
import           Data.Maybe (fromJust)
import qualified Data.Sequence as Sequence
import           Data.Functor ((<&>))
import           Data.List
import           Control.Monad (forM)

quote :: V.Ty -> ValState String
quote = \case
  V.TyLam base i cls -> do
    let vars = V.TyVar <$> [base .. base + i - 1]
    varTys <- mapM quote vars
    (cls $$ vars >>= quote)
      <&> ("Forall(" <> intercalate "," varTys <> ")." ++)
  V.TyArrow tys ty   -> do
    tys' <- mapM quote tys
    ty' <- quote ty
    pure $ "(" ++ intercalate "," tys' ++ ") -> " ++ ty'
  V.TyTuple tys      -> do
    tys' <- mapM quote tys
    pure $ "(" ++ intercalate "," tys' ++ ")"
  V.TyRcd tys        -> do
    tys' <- mapM (secondM quote) tys
    pure $ "{" ++ intercalate "," (map (\(k, v) -> k ++ ":" ++ v) tys') ++ "}"
  V.TyPrim p         -> pure $ show p
  V.TyVar i          -> do
    constr <- gets (fromJust . Sequence.lookup i . V.constrs)
    if null constr
      then fresh i
      else do
        n <- fresh i <&> show
        s <- forM constr
          $ \case
            Bot a -> eval a >>= quote
            Top a -> eval a >>= quote
        pure $ n ++ "[" ++ intercalate " + " s ++ "]"
  V.TyApp ty tys     -> do
    ty' <- quote ty
    tys' <- mapM quote tys
    pure $ ty' ++ "<" ++ intercalate "," tys' ++ ">"

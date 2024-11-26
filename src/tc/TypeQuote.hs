{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{-# LANGUAGE LambdaCase #-}

module TypeQuote where

import qualified Val as V
import           State (ValState, fresh)
import           Subtyping (($$), eval)
import           Utils (secondM, Constraint(..))
import           Control.Monad.State (gets)
import           Data.Maybe (fromJust)
import qualified Data.Sequence as Sequence
import           Data.Functor ((<&>))
import           Data.List (intercalate)
import           Control.Monad (forM)

quote :: V.Ty -> ValState String
quote = \case
  V.TyLam base i cls -> do
    let indices = [base .. base + i - 1]
    let vars = V.TyVar <$> indices
    let aux j = do
          constr <- gets (fromJust . Sequence.lookup j . V.constrs)
          if null constr
            then fresh j
            else do
              n <- fresh j
              s <- forM constr
                $ \case
                  Bot a -> eval a >>= quote
                  Top a -> eval a >>= quote
              pure $ n ++ ":" ++ intercalate " ∩ " s
    varTys <- mapM aux indices
    (cls $$ vars >>= quote) <&> ("∀" <> intercalate ", " varTys <> ".\n" ++)
  V.TyArrow [ty] ty' -> do
    tyQ' <- quote ty'
    tyQ <- quote ty
    pure $ tyQ ++ " → " ++ tyQ'
  V.TyArrow tys ty   -> do
    tys' <- mapM quote tys
    ty' <- quote ty
    pure $ "(" ++ intercalate ", " tys' ++ ") → " ++ ty'
  V.TyTuple tys      -> do
    tys' <- mapM quote tys
    pure $ "(" ++ intercalate ", " tys' ++ ")"
  V.TyRcd tys        -> do
    tys' <- mapM (secondM quote) tys
    pure $ "{" ++ intercalate ", " (map (\(k, v) -> k ++ ":" ++ v) tys') ++ "}"
  V.TyPrim p         -> pure $ show p
  V.TyVar i          -> fresh i
  V.TyApp ty tys     -> do
    ty' <- quote ty
    tys' <- mapM quote tys
    pure $ ty' ++ "<" ++ intercalate ", " tys' ++ ">"

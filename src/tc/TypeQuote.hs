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
import           Control.Monad (forM, unless)
import           Control.Monad.Error.Class (MonadError(throwError))
import           Errors (VTError(AmbiguousType))

quote :: V.Ty -> ValState String
quote = \case
  V.TyLam base i cls -> do
    let indices = [base .. base + i - 1]
    let vars = V.TyVar <$> indices
    names <- mapM fresh indices
    let aux j = do
          constr <- gets (fromJust . Sequence.lookup j . V.constrs)
          if null constr
            then pure []
            else do
              s <- forM constr
                $ \case
                  Bot a -> eval a >>= quote
                  Top a -> eval a >>= quote
              pure $ ":" ++ intercalate " ∩ " s
    tys <- mapM aux indices
    let varTys = zipWith (++) names tys
    (cls $$ vars >>= quote) <&> ("∀" <> unwords varTys <> " => " ++)
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
  V.TyVar i          -> do
    l0 <- gets (length . V.fresh)
    name <- fresh i
    l1 <- gets (length . V.fresh)
    unless (l0 == l1) $ throwError $ AmbiguousType (V.TyVar i)
    return name
  V.TyApp ty tys     -> do
    ty' <- quote ty
    tys' <- mapM quote tys
    pure $ ty' ++ "<" ++ intercalate ", " tys' ++ ">"

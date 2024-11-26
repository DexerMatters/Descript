{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Program where

import qualified Raw as R
import           State (putTermDef, putTypeDef, runEnvState, ProgState)
import           Raw (Prog(Prog))
import           Control.Monad.State (MonadState(get))
import           Errors (ProgError)
import           Control.Applicative (Alternative(empty))

scanForDefs :: R.Prog -> ProgState R.Symbols
scanForDefs (Prog defs) = mapM_ scanForDef defs >> get
  where
    scanForDef = \case
      R.ValDef x tm -> putTermDef x tm
      R.TyLet x ty -> putTypeDef x ty
      R.FuncDef x args ret body -> putTermDef x $ R.Lam args ret body
      _ -> error "not yet implemented"

getDefinitions :: R.Prog -> Either ProgError R.Symbols
getDefinitions = fst . runEnvState (R.Symbols empty empty) . scanForDefs
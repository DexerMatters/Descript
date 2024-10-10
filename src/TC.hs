{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module TC where

import Control.Arrow (Arrow (first, second))
import Control.Monad (mapAndUnzipM, unless, zipWithM)
import Data.List (elemIndex, findIndex)
import qualified Raw as R (Lit (..), Name, Pttrn (..), Tm (..), Ty (..))
import Tm (emptyConstr, (<$), (<^))
import qualified Tm as T (Constr (..), Prim (..), Pttrn (..), Ty (..))
import Val (Closure (..))
import qualified Val as V (Ty (..))
import Prelude hiding ((<$))

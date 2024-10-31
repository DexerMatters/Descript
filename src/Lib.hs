{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module Lib (someFunc) where

import           Dbg
import           Control.Arrow (Arrow(second))
import           Control.Monad ((>=>))
import           Control.Category ((>>>))

path :: String
path = "/home/dexer/Repos/haskell/descript/demo/test.ds"

someFunc :: IO ()
someFunc = printInfo path
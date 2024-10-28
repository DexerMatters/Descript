{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Lib (someFunc) where

import           Dbg

path :: String
path = "/home/dexer/Repos/haskell/descript/demo/test.ds"

someFunc :: IO ()
someFunc = do
  printInfo "Hello, World!"

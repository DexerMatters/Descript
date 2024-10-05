module Lib
  ( someFunc,
  )
where

import Parse (parseTm)
import Text.Megaparsec (parseTest)

path :: String
path = "/home/dexer/Repos/haskell/descript/demo/test.ds"

someFunc :: IO ()
someFunc = do
  raw <- readFile path
  parseTest (parseTm 0) raw

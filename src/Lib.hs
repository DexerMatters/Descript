module Lib
  ( someFunc,
  )
where

import Parse (parseTm)
import TC (infer)
import Text.Megaparsec (parseTest, runParser)
import Tm (PrettyShow (prettyShow))
import Utils (TCArrow (runTCArrow), runTC)

path :: String
path = "/home/dexer/Repos/haskell/descript/demo/test.ds"

someFunc :: IO ()
someFunc = do
  putStrLn "\ESC[92m"
  raw <- readFile path
  let parsed = runParser (parseTm 0) path raw
  case parsed of
    Left e -> do
      print e
    Right r -> do
      putStrLn $ show r ++ "\n"
      case runTC infer r of
        Left e -> print e
        Right r' ->
          print $ prettyShow r'

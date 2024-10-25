{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Lib
  ( someFunc,
  )
where

import Parse (parseTm)
import TC (infer)
import qualified TE
import TEUtils (liftEnv)
import Text.Megaparsec (parseTest, runParser)
import Utils (PartialArrow (runPartialArrow), PrettyShow (prettyShow), runPartially)

path :: String
path = "/home/dexer/Repos/haskell/descript/demo/test.ds"

printInfo :: String -> IO ()
printInfo x = putStrLn $ "\ESC[92m[Info]\t" ++ x ++ "\ESC[0m"

printErr :: String -> IO ()
printErr x = putStrLn $ "\ESC[91m[Error]\t" ++ x ++ "\ESC[0m"

someFunc :: IO ()
someFunc = do
  raw <- readFile path
  parseTest (parseTm 0) raw
  let parsed = runParser (parseTm 0) path raw
  case parsed of
    Left e -> do
      print e
    Right r -> do
      putStrLn $ show r ++ "\n"
      case runPartially infer r of
        Left e -> print e
        Right (a, env) -> do
          let env' = liftEnv env
          case runPartialArrow TE.eval (env', a) of
            Left e -> printErr $ show e
            Right (_, b) -> do
              printInfo $ prettyShow b

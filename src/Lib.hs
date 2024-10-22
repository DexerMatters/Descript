{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Lib
  ( someFunc,
  )
where

import Parse (parseTm)
import TC (infer)
import qualified TE
import TEUtils (liftEnv)
import Text.Megaparsec (parse, parseTest, runParser)
import Utils (PartialArrow (runPartialArrow), PrettyShow (prettyShow), runPartially)
import Val (Env (constrs))

path :: String
path = "/home/dexer/Repos/haskell/descript/demo/test.ds"

someFunc :: IO ()
someFunc = do
  putStrLn "\ESC[92m"
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
            Left e -> print e
            Right (env', b) -> do
              putStrLn $ prettyShow (constrs env')
              putStrLn $ prettyShow b

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Lib
  ( someFunc,
  )
where

import qualified Context as Ctx
import Parse (parseProg)
import TC (infer)
import TCUtils (Env (Env))
import qualified TE
import TEUtils (liftEnv)
import Text.Megaparsec (parseTest, runParser)
import Utils (PartialArrow (runPartialArrow), PrettyShow (prettyShow), runWithEnv)

path :: String
path = "/home/dexer/Repos/haskell/descript/demo/test.ds"

printInfo :: String -> IO ()
printInfo x = putStrLn $ "\ESC[92m[Info]\t" ++ x ++ "\ESC[0m"

printErr :: String -> IO ()
printErr x = putStrLn $ "\ESC[91m[Error]\t" ++ x ++ "\ESC[0m"

someFunc :: IO ()
someFunc = do
  raw <- readFile path
  parseTest parseProg raw
  let parsed = runParser parseProg path raw
  case parsed of
    Left e -> do
      print e
    Right r -> do
      putStrLn $ show r ++ "\n"
      let dumped = Ctx.dumpProgram r
      let env0 = Env [] [] [] dumped
      let exprs = Ctx.getTestEntrance dumped
      case runWithEnv infer env0 exprs of
        Left e -> print e
        Right (a, env) -> do
          let env' = liftEnv env
          case runPartialArrow TE.eval (env', a) of
            Left e -> printErr $ show e
            Right (_, b) -> do
              printInfo $ prettyShow b

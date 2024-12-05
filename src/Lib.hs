{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Lib (someFunc, check) where

import           Dbg
import           Text.Megaparsec (runParser, parse)
import           Parse (parseProg)
import           State
import           TypeInfer
import           Subtyping
import           Program (getDefinitions)
import           Raw (Symbols(terms))
import           Utils
import           TypeQuote (quote)

path :: String
path = "/home/dexer/Repos/haskell/descript/samples/test.ds"

someFunc :: IO ()
someFunc = do
  code <- readFile path
  check code

check :: String -> IO ()
check input = do
  printInfo "Parsing program:"
  putStrLn $ "\t>>> " <> input
  let prog = parse parseProg "" input
  case prog of
    Left err   -> printErr $ show err
    Right prog -> do
      let (Right defs) = getDefinitions prog
      -- printInfo $ "Parsed program:\n" ++ show defs
      case runTmState defs (infer (terms defs !!! "main")) of
        (Left err, _)   -> printErr $ show err
        (Right ty, ctx) -> do
          printInfo "Inferred type:"
          putStrLn $ "\t<<< " <> show ty
          case runValState ctx (eval ty) of
            (Left err, _) -> printErr $ show err
            (Right ty, _) -> do
              printInfo "Evaluated type:"
              putStrLn $ "\t<<< " <> show ty
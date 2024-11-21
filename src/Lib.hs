{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Lib (someFunc) where

import           Dbg
import           Text.Megaparsec (runParser)
import           Parse (parseTm, parseProg)
import           Control.Monad.Except
import           Control.Monad.RWS.Lazy (MonadTrans(lift), MonadIO(liftIO))
import           Data.Either (fromRight)
import           State
import           TypeInfer
import           Subtyping
import           Program (getDefinitions)
import           Raw (Symbols(terms))
import           Utils

path :: String
path = "/home/dexer/Repos/haskell/descript/demo/test.ds"

someFunc :: IO ()
someFunc = do
  str <- readFile path
  let prog = runParser parseProg path str
  case prog of
    Left err   -> printErr $ show err
    Right prog -> do
      let (Right defs) = getDefinitions prog
      printInfo $ "Parsed program:\n" ++ show defs
      case runTmState defs (infer (terms defs !!! "main")) of
        (Left err, _)   -> printErr $ show err
        (Right ty, ctx) -> do
          putStrLn "----------------------------"
          printInfo $ "Inferred type:\n" ++ show ty
          printInfo $ "Context:\n" ++ show ctx
          case runValState ctx (eval ty) of
            (Left err, _)    -> printErr $ show err
            (Right ty, ctx') -> do
              putStrLn "----------------------------"
              printInfo $ "Evaluated type:\n" ++ show ty
              printInfo $ "Context:\n" ++ show ctx'

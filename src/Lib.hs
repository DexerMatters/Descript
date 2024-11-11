{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE LambdaCase #-}

module Lib (someFunc) where

import           Dbg
import           Text.Megaparsec (runParser)
import           Parse (parseTm)
import           Control.Monad.Except
import           Control.Monad.RWS.Lazy (MonadTrans(lift), MonadIO(liftIO))
import           Data.Either (fromRight)
import           State
import           Checker
import           TypeEval (eval)

path :: String
path = "/home/dexer/Repos/haskell/descript/demo/test.ds"

someFunc :: IO ()
someFunc = do
  str <- readFile path
  let tm = runParser (parseTm 0) path str
  case tm of
    Left err -> printErr $ show err
    Right tm -> do
      printInfo $ "Parsed term:\n" ++ show tm
      case runTCState (infer tm) of
        (Left err, _)   -> printErr $ show err
        (Right ty, ctx) -> do
          putStrLn "----------------------------"
          printInfo $ "Inferred type:\n" ++ show ty
          printInfo $ "Context:\n" ++ show ctx
          case runTEState (eval ty) ctx of
            (Left err, _)    -> printErr $ show err
            (Right ty, ctx') -> do
              putStrLn "----------------------------"
              printInfo $ "Evaluated type:\n" ++ show ty
              printInfo $ "Context:\n" ++ show ctx'

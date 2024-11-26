{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Dbg where

import           Debug.Trace (trace)

makeLog :: ESCColor -> String -> String -> String
makeLog color title content =
  "\ESC[" ++ show color ++ "m[" ++ title ++ "]\t" ++ content ++ "\ESC[0m"

printInfo :: String -> IO ()
printInfo = putStrLn . makeLog Green "Info"

printErr :: String -> IO ()
printErr = putStrLn . makeLog Red "Error"

printWarn :: String -> IO ()
printWarn = putStrLn . makeLog Yellow "Warning"

traceInfo :: (Show a) => a -> a
traceInfo x = trace (makeLog Green "Info" (show x)) x

traceErr :: (Show a) => a -> a
traceErr x = trace (makeLog Red "Error" (show x)) x

printM :: (Applicative m) => String -> m ()
printM x = trace (makeLog Grey "Info" x) (pure ())

data ESCColor =
    Black
  | Grey
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Reset

instance Show ESCColor where
  show Black = "30"
  show Grey = "90"
  show Red = "31"
  show Green = "32"
  show Yellow = "33"
  show Blue = "34"
  show Magenta = "35"
  show Cyan = "36"
  show White = "37"
  show Reset = "0"
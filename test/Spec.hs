{-# LANGUAGE QuasiQuotes #-}

import           Data.Sequence (fromList)
import           Text.RawString.QQ
import           Lib (check)
import           Dbg (printInfo, printWarn)

main :: IO ()
main = do
  printInfo "\nRunning test"
  printWarn ">>> Case 1 - Primitive types <<<"
  check [r|let main = [1, true, "hello"]|]
  printWarn ">>> Case 2 - Function types <<<"
  check [r|let main = (x: Number, y: Bool) => [x, y]|]
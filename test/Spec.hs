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
  printWarn ">>> Case 3 - Destruction & Pattern <<<"
  check
    [r|
      let test = ([a, b] : [Number, Bool]) => b
      let main = test([1, true])
    |]
  printWarn ">>> Case 4 - Parameter Polymorphism <<<"
  check
    [r|
      let f = (x, y) => [x, y]
      let main = [f(1, true), f]
    |]
  printWarn ">>> Case 5 - Constraint Deduction <<<"
  check [r|
      let f = (x) => x as Number
      let main = f
    |]
  printWarn ">>> Case 6 - Record Polymorphism <<<"
  check
    [r|
      let f = (x) => x.a
      let main = [f({a = 1, b = true}), f]
    |]
  printWarn ">>> Case 7 - Deep Constraint Deduction <<<"
  check
    [r|
      let f = (x) => {x.a as Number; x.b as Bool; x}
      let main = [f({a = 1, b = true}), f]
    |]
  printWarn ">>> Case 8 - High-Order Function <<<"
  check
    [r|
      let f = (x, y) => (z) => [x, y, x]
      let main = (f(1, "hello"))(true)
    |]
  printWarn ">>> Case 9 - Let Polymorphism <<<"
  check
    [r|
      let main = (
        let f = (x) => x
        let v = f(1)
        f(v)
      )
    |]
  printWarn ">>> Case 10 - Function Constraint Polymorphism <<<"
  check
    [r|
      let g = (f) => {f(12); f(true)}
      let h = (x) => x
      let main = [g(h), g]
    |]
  printWarn ">>> Case 11 - Mutual Deduction <<<"
  check
    [r|
      let f = (x, a) => x(a)
      let g = (x) => (x)
      let main = [f(g, 12), f]
    |]
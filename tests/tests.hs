-- base
import System.Exit (exitSuccess, exitFailure)

-- HUnit
import Test.HUnit (runTestTT, errors, failures, Test (TestList), (~=?))

-- scilab
import Scilab.Parser
import Scilab.Interpreter

main :: IO ()
main
  = do
    counts_ <- runTestTT tests
    if errors counts_ + failures counts_ > 0
      then exitFailure
      else exitSuccess

tests :: Test
tests = TestList [parse, execution]

parse :: Test
parse
  = TestList
    [[CAttr (RVar "a") (ENumber 1.0)] ~=? parser "a = 1",
      [CAttr (RVI "a" (ENumber 1.0)) (ENumber 1.0)] ~=? parser "a(1) = 1",
      [CAttr (RVar "a") (EAdd (EAdd (ENumber 1.0) (ENumber 1.0)) (ENumber 1.0))]
        ~=? parser "a = 1 + 1 + 1",
      [CIf
          (EGT (EVar "a") (ENumber 10.0))
          [CAttr (RVar "a") (ENumber 20.0)]
          [CAttr (RVar "a") (ENumber 0.0)]]
        ~=? parser "if a > 10\n  a = 20\nelse\n  a = 0\nend"]

execution :: Test
execution
  = [Atom (AtomNumber 2.0)]
     ~=? interpret [toAtom (2 :: Double)] "a = input(\"\")\ndisp(a)"

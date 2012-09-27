-- base
import System.Exit (exitSuccess, exitFailure)

-- HUnit
import Test.HUnit (runTestTT, errors, failures, Test (TestList), (~=?))

-- scilab
import Scilab.Parser

main :: IO ()
main
  = do
    counts_ <- runTestTT tests
    if errors counts_ + failures counts_ > 0
      then exitFailure
      else exitSuccess

tests :: Test
tests
  = TestList
    [[CAttr (RVar "a") (ENumber 1.0)] ~=? parser "a = 1",
      [CAttr (RVI "a" (ENumber 1.0)) (ENumber 1.0)] ~=? parser "a(1) = 1",
      [CAttr (RVar "a") (EAdd (EAdd (ENumber 1.0) (ENumber 1.0)) (ENumber 1.0))]
        ~=? parser "a = 1 + 1 + 1"]


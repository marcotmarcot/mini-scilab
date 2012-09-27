-- base
import System.Exit (exitSuccess, exitFailure)
import Data.Monoid ((<>))

-- vector
import qualified Data.Vector as V

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
        ~=? parser "if a > 10\n  a = 20\nelse\n  a = 0\nend",
      [CAttr (RVar "a") (EVecFromTo (ENumber 1.0) (ENumber 10.0))]
        ~=? parser "a = 1 : 10",
      [CAttr
          (RVar "a")
          (EVecFromToStep (ENumber 1.0) (ENumber 5.0) (ENumber 10.0))]
        ~=? parser "a = 1 : 5 : 10",
      [CAttr (RVar "a") (ENumber 1.0)] ~=? parser "a = (1)"]

execution :: Test
execution
  = TestList
    [[Atom (AtomNumber 2.0)]
        ~=? interpret [toAtom (2 :: Double)] "a = input(\"\")\ndisp(a)",
      [Vec (VecNumber (V.fromList [1.0,2.0,3.0,4.0]))]
        ~=? interpret [] "disp(1 : 4)",
      [Vec (VecNumber (V.fromList [1.0,3.0,5.0,7.0]))]
        ~=? interpret [] "disp(1 : 2 : 7)",
      [Vec (VecNumber (V.fromList []))]
        ~=? interpret [] "disp(3 : 1)",
      [Vec (VecNumber (V.fromList [3.0,2.0,1.0]))]
        ~=? interpret [] "disp(3 : -1 : 1)",
      [Atom (AtomNumber 450.0)]
        ~=? interpret
          (map toAtom [11 :: Double, 29, 46, 47, 57, 24, 50, 92, 10, 84])
          ("total = 0\n"
            <> "i = 1\n"
            <> "while i <= 10 do\n"
            <> "  v = input(\"\")\n"
            <> "  total = total + v\n"
            <> "  i = i + 1\n"
            <>  "end\n"
            <> "disp(total)"),
      [Vec
          (VecNumber
            (V.fromList [5.5,14.5,23.0,23.5,28.5,12.0,25.0,46.0,5.0,42.0]))]
        ~=? interpret
          (map toAtom [11 :: Double, 29, 46, 47, 57, 24, 50, 92, 10, 84])
          ("v = []\n"
            <> "i = 1\n"
            <> "while i <= 10 do\n"
            <> "  n = input(\"\")\n"
            <> "  v = [v n]\n"
            <> "  i = i + 1\n"
            <>  "end\n"
            <> "disp(v / 2)"),
      [Vec
          (VecNumber
            (V.fromList [5.5,14.5,23.0,23.5,28.5,12.0,25.0,46.0,5.0,42.0]))]
        ~=? interpret
          (map toAtom [11 :: Double, 29, 46, 47, 57, 24, 50, 92, 10, 84])
          ("v = []\n"
            <> "for i = 1 : 10 do\n"
            <> "  n = input(\"\")\n"
            <> "  v = [v n]\n"
            <>  "end\n"
            <> "disp(v / 2)")]

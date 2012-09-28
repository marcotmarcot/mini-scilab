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
    [[scalarD 2]
        ~=? interpret [scalarD 2] "a = input(\"\")\ndisp(a)",
      [vecL [1 .. 4]] ~=? interpret [] "disp(1 : 4)",
      [vecL [1 ,3 .. 7]] ~=? interpret [] "disp(1 : 2 : 7)",
      [vecL []] ~=? interpret [] "disp(3 : 1)",
      [vecL [3, 2, 1]] ~=? interpret [] "disp(3 : -1 : 1)",
      [scalarD 450]
        ~=? interpret
          (map scalarD [11, 29, 46, 47, 57, 24, 50, 92, 10, 84])
          ("total = 0\n"
            <> "i = 1\n"
            <> "while i <= 10 do\n"
            <> "  v = input(\"\")\n"
            <> "  total = total + v\n"
            <> "  i = i + 1\n"
            <>  "end\n"
            <> "disp(total)"),
      [vecL [5.5, 14.5, 23, 23.5, 28.5, 12, 25, 46, 5, 42]]
        ~=? interpret
          (map scalarD [11, 29, 46, 47, 57, 24, 50, 92, 10, 84])
          ("v = []\n"
            <> "i = 1\n"
            <> "while i <= 10 do\n"
            <> "  n = input(\"\")\n"
            <> "  v = [v n]\n"
            <> "  i = i + 1\n"
            <>  "end\n"
            <> "disp(v / 2)"),
      [vecL [5.5, 14.5, 23, 23.5, 28.5, 12, 25, 46, 5, 42]]
        ~=? interpret
          (map scalarD [11, 29, 46, 47, 57, 24, 50, 92, 10, 84])
          ("v = []\n"
            <> "for i = 1 : 10 do\n"
            <> "  n = input(\"\")\n"
            <> "  v = [v n]\n"
            <>  "end\n"
            <> "disp(v / 2)"),
      [vecL
          [3.3166247903554,
            5.385164807134504,
            6.782329983125268,
            6.855654600401044,
            7.54983443527075,
            4.898979485566356,
            7.0710678118654755,
            9.591663046625438,
            3.1622776601683795,
            9.16515138991168,
            -1,
            -1,
            -1,
            -1,
            -1]]
        ~=? interpret
          (map scalarD
            [11, 29, 46, 47, 57, 24, 50, 92, 10, 84, -18, -29, -25, -40, -35])
          ("v = []\n"
            <> "for i = 1 : 15 do\n"
            <> "  n = input(\"\")\n"
            <> "  if n < 0 then\n"
            <> "    n = -1\n"
            <> "  else\n"
            <> "    n = sqrt(n)\n"
            <> "  end\n"
            <> "  v = [v n]\n"
            <>  "end\n"
            <> "disp(v)"),
      [scalarD (-1)] ~=? interpret [] "a = 1; disp(-a)",
      [vecL [-1, -1, -1]] ~=? interpret [] "disp(-1^[1 2 3])",
      [vecL [-1, 1, -1]] ~=? interpret [] "disp((-1)^[1 2 3])",
      [scalarD 2] ~=? interpret [] "a = [1]\na(1) = 2\ndisp(a(1))",
      [scalarD 2] ~=? interpret [] "a = 1\na(1) = 2\ndisp(a(1))",
      (map scalarD [1, 2])
        ~=? interpret [] "i = 1\nwhile [i <= 2]\n  disp(i)\n  i = i + 1\nend",
      [scalarD 1] ~=? interpret [] "for x = 1 do\n  disp(x)\nend"]

vecL :: [Double] -> Value
vecL = vec . V.fromList

scalarD :: Double -> Value
scalarD = scalar

-- base
import System.Exit (exitSuccess, exitFailure)
import Data.Monoid ((<>))
import Control.Exception (evaluate)
import System.Timeout (timeout)

-- deepseq
import Control.DeepSeq (force)

-- vector
import qualified Data.Vector as V

-- HUnit
import
  Test.HUnit
  (runTestTT, errors, failures, Test (TestList, TestCase), (~=?), (@=?))

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
tests = TestList [parse, execution, loop, others]

parse :: Test
parse
  = TestList
    [[CAttr (RVar "a") (ENumber 1.0)] ~=? parser "a = 1",
      [CAttr (RVI "a" [ENumber 1.0]) (ENumber 1.0)] ~=? parser "a(1) = 1",
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
    [([], [2]) ~=? interpret [2] "a = input(\"\")\ndisp(a)",
      ([], [1 .. 4]) ~=? interpret [] "disp(1 : 4)",
      ([], [1 ,3 .. 7]) ~=? interpret [] "disp(1 : 2 : 7)",
      ([], []) ~=? interpret [] "disp(3 : 1)",
      ([], [3, 2, 1]) ~=? interpret [] "disp(3 : -1 : 1)",
      ([], [450])
        ~=? interpret
          [11, 29, 46, 47, 57, 24, 50, 92, 10, 84]
          ("total = 0\n"
            <> "i = 1\n"
            <> "while i <= 10 do\n"
            <> "  v = input(\"\")\n"
            <> "  total = total + v\n"
            <> "  i = i + 1\n"
            <>  "end\n"
            <> "disp(total)"),
      ([], [5.5, 14.5, 23, 23.5, 28.5, 12, 25, 46, 5, 42])
        ~=? interpret
          [11, 29, 46, 47, 57, 24, 50, 92, 10, 84]
          ("v = []\n"
            <> "i = 1\n"
            <> "while i <= 10 do\n"
            <> "  n = input(\"\")\n"
            <> "  v = [v n]\n"
            <> "  i = i + 1\n"
            <>  "end\n"
            <> "disp(v / 2)"),
      ([], [5.5, 14.5, 23, 23.5, 28.5, 12, 25, 46, 5, 42])
        ~=? interpret
          [11, 29, 46, 47, 57, 24, 50, 92, 10, 84]
          ("v = []\n"
            <> "for i = 1 : 10 do\n"
            <> "  n = input(\"\")\n"
            <> "  v = [v n]\n"
            <>  "end\n"
            <> "disp(v / 2)"),
      ([],
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
            -1])
        ~=? interpret
          [11, 29, 46, 47, 57, 24, 50, 92, 10, 84, -18, -29, -25, -40, -35]
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
      ([], [-1]) ~=? interpret [] "a = 1; disp(-a)",
      ([], [-1, -1, -1]) ~=? interpret [] "disp(-1^[1 2 3])",
      ([], [-1, 1, -1]) ~=? interpret [] "disp((-1)^[1 2 3])",
      ([], [2]) ~=? interpret [] "a = [1]\na(1) = 2\ndisp(a(1))",
      ([], [2]) ~=? interpret [] "a = 1\na(1) = 2\ndisp(a(1))",
      ([], [1, 2])
        ~=? interpret
          []
          "i = 1\nwhile [i <= 2]\n  disp(i)\n  i = i + 1\nend",
      ([], [1])
        ~=? interpret [] "for x = 1 do\n  disp(x)\nend",
      ([], [1])
        ~=? interpret [1] "a = input(\"\")\r\ndisp(a)",
      ([], [1, 2]) ~=? interpret [] "printf(\"\", 1, 2)",
      ([], [3]) ~=? interpret [] "a_b = 3; disp(a_b)",
      ([], [25.0,24.0,63.0,34.0,72.0,79.0,87.0,51.0,94.0,11.0])
        ~=? interpret
          [25.0,76.0,43.0,29.0,5.0,30.0,63.0,34.0,52.0,98.0,89.0,24.0,82.0,10.0,76.0,90.0,95.0,94.0,30.0,7.0,59.0,66.0,63.0,76.0,1.0,13.0,62.0,79.0,97.0,93.0,23.0,80.0,65.0,34.0,46.0,74.0,7.0,3.0,97.0,87.0,36.0,37.0,51.0,75.0,72.0,43.0,52.0,32.0,69.0,47.0,16.0,22.0,78.0,68.0,70.0,79.0,78.0,78.0,52.0,51.0,97.0,21.0,34.0,2.0,66.0,3.0,87.0,16.0,97.0,99.0,49.0,17.0,6.0,96.0,43.0,79.0,44.0,51.0,13.0,47.0,32.0,14.0,85.0,65.0,38.0,46.0,53.0,34.0,94.0,60.0,12.0,60.0,13.0,96.0,16.0,13.0,34.0,58.0,71.0,11.0]
          ("for i = 1 : 10; for j = 1 : 10; M(i, j) = input(); end; end;"
            <> "for i = 1 : 10; disp(M(i, i)); end"),
      ([], [24]) ~=? interpret [] "disp(prod(1 : 4))"]

others :: Test
others
  = TestList
    [Number
          False
          (V.replicate 11 0
            V.++ V.singleton 8
            V.++ V.replicate 14 0
            V.++ V.singleton 8
            V.++ V.replicate 3 0)
          6
        ~=? updateVector
          6
          2
          (Number False (V.singleton 8) 1)
          (Number False (V.replicate 14 0 V.++ V.singleton 8) 3),
      Number False (V.enumFromN 5 4) 4
        ~=? getIndex
          (V.enumFromN 1 4)
          (V.singleton 2)
          (Number False (V.enumFromN 1 8) 4),
      Number False (V.singleton 5) 1
        ~=? getIndex
          (V.singleton 1)
          (V.singleton 1)
          (Number False (V.singleton 5) 1),
      Number False (V.singleton 3) 1
        ~=? updateVector
          1
          1
          (Number False (V.singleton 3) 1)
          (Number False V.empty 1)]

loop :: Test
loop
  = TestCase
    $ do
      result
        <- timeout 100000
          $ evaluate
          $ force
          $ interpret [] "i = 1\nwhile i > 0 do\ni = i + 1\nend"
      Nothing @=? result

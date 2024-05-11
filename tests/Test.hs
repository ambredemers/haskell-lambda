module Main where

import Control.Monad.Trans.Either
import Control.Monad.Trans.State.Strict
import Data.Char
import qualified Data.Text as Text
import InterpretAnf
import InterpretTerm
import Parser
import qualified System.Exit as Exit
import Term
import Test.HUnit

-- term building helper functions
tFVar :: String -> Term
tFVar name = TermFvar (Text.pack name) emptyDbg

tBVar :: Int -> Term
tBVar index = TermBvar index (Text.pack (show index)) emptyDbg

tAbs :: [String] -> Term -> Term
tAbs vars body = TermAbs body [] (map Text.pack vars) emptyDbg

tApp :: Term -> [Term] -> Term
tApp fn args = TermApp fn args emptyDbg

tTrue :: Term
tTrue = TermBool True emptyDbg

tFalse :: Term
tFalse = TermBool False emptyDbg

tIf :: Term -> Term -> Term -> Term
tIf cond cnsq alt = TermIf cond cnsq alt emptyDbg

-- combinators
s :: Term
s = tAbs ["x", "y", "z"] (tApp (tApp (tBVar 2) [tBVar 0]) [tApp (tBVar 1) [tBVar 0]])

k :: Term
k = tAbs ["x", "y"] (tBVar 1)

i :: Term
i = tAbs ["y"] (tBVar 0)

-- tests
testskk :: Test
testskk = TestCase (assertEqual message expected actual)
  where
    expected = Right (tFVar "x") :: Either String Term
    actual = eval (tApp (tApp (tApp s [k]) [k]) [tFVar "x"]) (Text.pack "")
    message = "testskk: evaluation returned an unexpected value."

testi :: Test
testi = TestCase (assertEqual message expected actual)
  where
    expected = Right (tFVar "x") :: Either String Term
    actual = eval (tApp i [tFVar "x"]) (Text.pack "")
    message = "testi: evaluation returned an unexpected value."

testIfTrue :: Test
testIfTrue = TestCase (assertEqual message expected actual)
  where
    expected = Right (tFVar "x") :: Either String Term
    actual = eval (tIf tTrue (tFVar "x") (tFVar "y")) (Text.pack "")
    message = "testIfTrue: evaluation returned an unexpected value."

testIfFalse :: Test
testIfFalse = TestCase (assertEqual message expected actual)
  where
    expected = Right (tFVar "y") :: Either String Term
    actual = eval (tIf tFalse (tFVar "x") (tFVar "y")) (Text.pack "")
    message = "testIfFalse: evaluation returned an unexpected value."

testTokenize :: Test
testTokenize = TestCase (assertEqual message expected actual)
  where
    expected =
      [ TokLparen {tokLpDbg = Dbg {dStart = 0, dEnd = 1}},
        TokLambda {tokLambdaDbg = Dbg {dStart = 1, dEnd = 7}},
        TokLparen {tokLpDbg = Dbg {dStart = 8, dEnd = 9}},
        TokVar {tokVarName = Text.pack "left", tokVarDbg = Dbg {dStart = 9, dEnd = 13}},
        TokVar {tokVarName = Text.pack "right", tokVarDbg = Dbg {dStart = 14, dEnd = 19}},
        TokRparen {tokRpDbg = Dbg {dStart = 19, dEnd = 20}},
        TokLparen {tokLpDbg = Dbg {dStart = 21, dEnd = 22}},
        TokIf {tokIfDbg = Dbg {dStart = 22, dEnd = 24}},
        TokVar {tokVarName = Text.pack "left", tokVarDbg = Dbg {dStart = 25, dEnd = 29}},
        TokLparen {tokLpDbg = Dbg {dStart = 30, dEnd = 31}},
        TokIf {tokIfDbg = Dbg {dStart = 31, dEnd = 33}},
        TokVar {tokVarName = Text.pack "right", tokVarDbg = Dbg {dStart = 34, dEnd = 39}},
        TokBool {tokBoolValue = True, tokBoolDbg = Dbg {dStart = 40, dEnd = 45}},
        TokBool {tokBoolValue = False, tokBoolDbg = Dbg {dStart = 46, dEnd = 52}},
        TokRparen {tokRpDbg = Dbg {dStart = 52, dEnd = 53}},
        TokBool {tokBoolValue = False, tokBoolDbg = Dbg {dStart = 54, dEnd = 60}},
        TokRparen {tokRpDbg = Dbg {dStart = 60, dEnd = 61}},
        TokRparen {tokRpDbg = Dbg {dStart = 61, dEnd = 62}}
      ]
    actual = tokenize (Text.pack "(lambda (left right) (if left (if right #true #false) #false))")
    message = "testTokenize: evaluation returned an unexpected value."

-- testParseTerm :: Test
-- testParseTerm =
--     let expected =
--             (Left "", []) :: (Either String Term, [Token])
--     in let input = Text.pack "((lambda (left right) (if left (if right #true #false) #false)) #true #true)"
--     in let actual = evalState (runEitherT parseTerm) $ ParserState (tokenize input) [] input
--     in let message = "testParseTerm: evaluation returned an unexpected value."
--     in TestCase (assertEqual message expected actual)

testEvalStringI :: Test
testEvalStringI = TestCase (assertEqual message expected actual)
  where
    expected = "Right (lambda (x) x)"
    actual = show (evalString "(lambda (x) x)")
    message = "ESvaluation returned an unexpected value."

testEvalStringComplexLambda :: Test
testEvalStringComplexLambda = TestCase (assertEqual message expected actual)
  where
    expected = "Right (lambda (y) y)"
    actual = show (evalString "((lambda (x) (x x)) ((lambda (f) (f (f (lambda (y) y)))) (lambda (x) x)))")
    message = "Evaluation returned an unexpected value."

testEvalStringIfTrue :: Test
testEvalStringIfTrue = TestCase (assertEqual message expected actual)
  where
    expected = "Right x"
    actual = show (evalString "(if #true x y)")
    message = "Evaluation returned an unexpected value."

testEvalStringIfFalse :: Test
testEvalStringIfFalse = TestCase (assertEqual message expected actual)
  where
    expected = "Right x"
    actual = show (evalString "(if #true x y)")
    message = "Evaluation returned an unexpected value."

testExtraArgs :: Test
testExtraArgs = TestCase (assertEqual message expected actual)
  where
    expected = "Left \"Error at ((lambda (x) x) y z) (line 1, column 1):\\n\\tCould not evaluate function - expected 1 arguments, but got 2\""
    actual = show (evalString "((lambda (x) x) y z)")
    message = "An unexpected error message was returned."

testInvalidArgs :: Test
testInvalidArgs = TestCase (assertEqual message expected actual)
  where
    expected = "Left \"Error at ((lambda (x) x) y z) (line 1, column 17):\\n\\tCould not evaluate function - expected 1 arguments, but got 2\""
    actual = show (evalString "((lambda (x) x) ((lambda (x) x) y z))")
    message = "An unexpected error message was returned."

testAdd :: Test
testAdd = TestCase (assertEqual message expected actual)
  where
    expected = "Right 58"
    actual = show (evalString "(+ 100 -42)")
    message = "Evaluation returned an unexpected value."

testSub :: Test
testSub = TestCase (assertEqual message expected actual)
  where
    expected = "Right -3"
    actual = show (evalString "(- 7 10)")
    message = "Evaluation returned an unexpected value."

testMul :: Test
testMul = TestCase (assertEqual message expected actual)
  where
    expected = "Right 256"
    actual = show (evalString "(* 16 16)")
    message = "Evaluation returned an unexpected value."

testDiv :: Test
testDiv = TestCase (assertEqual message expected actual)
  where
    expected = "Right -3"
    actual = show (evalString "(/ -51 13)")
    message = "Evaluation returned an unexpected value."

testAddLeftBool :: Test
testAddLeftBool = TestCase (assertEqual message expected actual)
  where
    expected = "Left \"Error at (+ #true 1) (line 1, column 1):\\n\\tCould not evaluate +, expected left argument to be an integer but got #true\""
    actual = show (evalString "(+ #true 1)")
    message = "An unexpected error message was returned."

testSubRightFvar :: Test
testSubRightFvar = TestCase (assertEqual message expected actual)
  where
    expected = "Left \"Error at (- 0 x) (line 1, column 1):\\n\\tCould not evaluate -, expected right argument to be an integer but got x\""
    actual = show (evalString "(- 0 x)")
    message = "An unexpected error message was returned."

testMulLeftError :: Test
testMulLeftError = TestCase (assertEqual message expected actual)
  where
    expected = "Left \"Error at ((lambda (x) x) x y) (line 1, column 4):\\n\\tCould not evaluate function - expected 1 arguments, but got 2\""
    actual = show (evalString "(* ((lambda (x) x) x y) -1)")
    message = "An unexpected error message was returned."

testDivRightError :: Test
testDivRightError = TestCase (assertEqual message expected actual)
  where
    expected = "Left \"Error at ((lambda (x) x) x y) (line 1, column 6):\\n\\tCould not evaluate function - expected 1 arguments, but got 2\""
    actual = show (evalString "(/ 2 ((lambda (x) x) x y))")
    message = "An unexpected error message was returned."

testLt :: Test
testLt = TestCase (assertEqual message expected actual)
  where
    expected = "Right #false"
    actual = show (evalString "(< 0 0)")
    message = "Evaluation returned an unexpected value."

testLeq :: Test
testLeq = TestCase (assertEqual message expected actual)
  where
    expected = "Right #true"
    actual = show (evalString "(<= 0 0)")
    message = "Evaluation returned an unexpected value."

testEq :: Test
testEq = TestCase (assertEqual message expected actual)
  where
    expected = "Right #true"
    actual = show (evalString "(= 0 0)")
    message = "Evaluation returned an unexpected value."

testGeq :: Test
testGeq = TestCase (assertEqual message expected actual)
  where
    expected = "Right #true"
    actual = show (evalString "(>= 0 0)")
    message = "Evaluation returned an unexpected value."

testGt :: Test
testGt = TestCase (assertEqual message expected actual)
  where
    expected = "Right #false"
    actual = show (evalString "(> 0 0)")
    message = "Evaluation returned an unexpected value."

testNeq :: Test
testNeq = TestCase (assertEqual message expected actual)
  where
    expected = "Right #false"
    actual = show (evalString "(/= 0 0)")
    message = "Evaluation returned an unexpected value."

testUnit :: Test
testUnit = TestCase (assertEqual message expected actual)
  where
    expected = "Right ()"
    actual = show (evalString "()")
    message = "Evaluation returned an unexpected value."

testLetAdd :: Test
testLetAdd = TestCase (assertEqual message expected actual)
  where
    expected = "Right 3"
    actual = show (evalString "(let ((a 1) (b 2)) (+ a b))")
    message = "Evaluation returned an unexpected value."

testLetIfApp :: Test
testLetIfApp = TestCase (assertEqual message expected actual)
  where
    expected = "Right a"
    actual = show (evalString "(let ((f (lambda (x y z) (if x y z))) (z #true) (y a) (x b)) (f z y x))")
    message = "Evaluation returned an unexpected value."

testLetShadowing :: Test
testLetShadowing = TestCase (assertEqual message expected actual)
  where
    expected = "Right 1"
    actual = show (evalString "(let ((a 0) (a (+ a 1))) a)")
    message = "Evaluation returned an unexpected value."

testLetParseExpectRparenButEOF :: Test
testLetParseExpectRparenButEOF = TestCase (assertEqual message expected actual)
  where
    expected = "Left \"Error at  (line 1, column 15):\\n\\tParsing error: expected ')' but reached end of file\""
    actual = show (evalString "(let ((a 0)) a")
    message = "An unexpected error message was returned."

testEvalAnfStringComplexLambda :: Test
testEvalAnfStringComplexLambda = TestCase (assertEqual message expected actual)
  where
    expected = "Right (lambda (y) y)"
    actual = show (evalAnfString "((lambda (x) (x x)) ((lambda (f) (f (f (lambda (y) y)))) (lambda (x) x)))")
    message = "Evaluation returned an unexpected value."

tests =
  TestList
    [ TestLabel "testskk" testskk,
      TestLabel "tessti" testi,
      TestLabel "testIfTrue" testIfTrue,
      TestLabel "testIfFalse" testIfFalse,
      TestLabel "testTokenize" testTokenize,
      TestLabel "testEvalStringI" testEvalStringI,
      TestLabel "testEvalStringComplexLambda" testEvalStringComplexLambda,
      TestLabel "testEvalStringIfTrue" testEvalStringIfTrue,
      TestLabel "testEvalStringIfFalse" testEvalStringIfFalse,
      TestLabel "testExtraArgs" testExtraArgs,
      TestLabel "testInvalidArgs" testInvalidArgs,
      TestLabel "testAdd" testAdd,
      TestLabel "testSub" testSub,
      TestLabel "testMul" testMul,
      TestLabel "testDiv" testDiv,
      TestLabel "testLt" testLt,
      TestLabel "testLeq" testLeq,
      TestLabel "testEq" testEq,
      TestLabel "testGeq" testGeq,
      TestLabel "testGt" testGt,
      TestLabel "testNeq" testNeq,
      TestLabel "testAddLeftBool" testAddLeftBool,
      TestLabel "testSubRightFvar" testSubRightFvar,
      TestLabel "testMulLeftError" testMulLeftError,
      TestLabel "testDivRightError" testDivRightError,
      TestLabel "testLetAdd" testLetAdd,
      TestLabel "testLetIfApp" testLetIfApp,
      TestLabel "testLetShadowing" testLetShadowing,
      TestLabel "testLetParseExpectRparenButEOF" testLetParseExpectRparenButEOF,
      TestLabel "testEvalAnfStringComplexLambda" testEvalAnfStringComplexLambda
    ]

main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
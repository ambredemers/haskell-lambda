module Main where

import Control.Monad.Trans.Either
import Control.Monad.Trans.State.Strict
import Data.Char
import qualified Data.Text as Text
import InterpretAnf
import Interpreter
import Parser
import qualified System.Exit as Exit
import Term
import Test.HUnit

-- term building helper functions
tFVar :: String -> Term
tFVar name = TFVar (Text.pack name) emptyDbg

tBVar :: Int -> Term
tBVar index = TBVar index (Text.pack (show index)) emptyDbg

tAbs :: [String] -> Term -> Term
tAbs vars body = TAbs body [] (map Text.pack vars) emptyDbg

tApp :: Term -> [Term] -> Term
tApp fn args = TApp fn args emptyDbg

tTrue :: Term
tTrue = TBool True emptyDbg

tFalse :: Term
tFalse = TBool False emptyDbg

tIf :: Term -> Term -> Term -> Term
tIf cond cnsq alt = TIf cond cnsq alt emptyDbg

-- combinators
s :: Term
s = tAbs ["x", "y", "z"] (tApp (tApp (tBVar 2) [tBVar 0]) [tApp (tBVar 1) [tBVar 0]])

k :: Term
k = tAbs ["x", "y"] (tBVar 1)

i :: Term
i = tAbs ["y"] (tBVar 0)

-- tests
testskk :: Test
testskk =
  let expected = Right (tFVar "x") :: Either String Term
   in let actual = eval (tApp (tApp (tApp s [k]) [k]) [tFVar "x"]) (Text.pack "")
       in let message = "testskk: evaluation returned an unexpected value."
           in TestCase (assertEqual message expected actual)

testi :: Test
testi =
  let expected = Right (tFVar "x") :: Either String Term
   in let actual = eval (tApp i [tFVar "x"]) (Text.pack "")
       in let message = "testi: evaluation returned an unexpected value."
           in TestCase (assertEqual message expected actual)

testIfTrue :: Test
testIfTrue =
  let expected = Right (tFVar "x") :: Either String Term
   in let actual = eval (tIf tTrue (tFVar "x") (tFVar "y")) (Text.pack "")
       in let message = "testIfTrue: evaluation returned an unexpected value."
           in TestCase (assertEqual message expected actual)

testIfFalse :: Test
testIfFalse =
  let expected = Right (tFVar "y") :: Either String Term
   in let actual = eval (tIf tFalse (tFVar "x") (tFVar "y")) (Text.pack "")
       in let message = "testIfFalse: evaluation returned an unexpected value."
           in TestCase (assertEqual message expected actual)

testTokenize :: Test
testTokenize =
  let expected =
        [ Lparen {lpDbg = Dbg {dStart = 0, dEnd = 1}},
          ToLambda {laDbg = Dbg {dStart = 1, dEnd = 7}},
          Lparen {lpDbg = Dbg {dStart = 8, dEnd = 9}},
          ToVar {toVName = Text.pack "left", toVDbg = Dbg {dStart = 9, dEnd = 13}},
          ToVar {toVName = Text.pack "right", toVDbg = Dbg {dStart = 14, dEnd = 19}},
          Rparen {rpDbg = Dbg {dStart = 19, dEnd = 20}},
          Lparen {lpDbg = Dbg {dStart = 21, dEnd = 22}},
          ToIf {toIfDbg = Dbg {dStart = 22, dEnd = 24}},
          ToVar {toVName = Text.pack "left", toVDbg = Dbg {dStart = 25, dEnd = 29}},
          Lparen {lpDbg = Dbg {dStart = 30, dEnd = 31}},
          ToIf {toIfDbg = Dbg {dStart = 31, dEnd = 33}},
          ToVar {toVName = Text.pack "right", toVDbg = Dbg {dStart = 34, dEnd = 39}},
          ToTrue {toTDbg = Dbg {dStart = 40, dEnd = 45}},
          ToFalse {toFDbg = Dbg {dStart = 46, dEnd = 52}},
          Rparen {rpDbg = Dbg {dStart = 52, dEnd = 53}},
          ToFalse {toFDbg = Dbg {dStart = 54, dEnd = 60}},
          Rparen {rpDbg = Dbg {dStart = 60, dEnd = 61}},
          Rparen {rpDbg = Dbg {dStart = 61, dEnd = 62}}
        ]
   in let actual = tokenize (Text.pack "(lambda (left right) (if left (if right #true #false) #false))")
       in let message = "testTokenize: evaluation returned an unexpected value."
           in TestCase (assertEqual message expected actual)

-- testParseTerm :: Test
-- testParseTerm =
--     let expected =
--             (Left "", []) :: (Either String Term, [Token])
--     in let input = Text.pack "((lambda (left right) (if left (if right #true #false) #false)) #true #true)"
--     in let actual = evalState (runEitherT parseTerm) $ ParserState (tokenize input) [] input
--     in let message = "testParseTerm: evaluation returned an unexpected value."
--     in TestCase (assertEqual message expected actual)

testEvalStringI :: Test
testEvalStringI =
  let expected = "Right (lambda (x) x)"
   in let actual = show (evalString "(lambda (x) x)")
       in let message = "ESvaluation returned an unexpected value."
           in TestCase (assertEqual message expected actual)

testEvalStringComplexLambda :: Test
testEvalStringComplexLambda =
  let expected = "Right (lambda (y) y)"
   in let actual = show (evalString "((lambda (x) (x x)) ((lambda (f) (f (f (lambda (y) y)))) (lambda (x) x)))")
       in let message = "Evaluation returned an unexpected value."
           in TestCase (assertEqual message expected actual)

testEvalStringIfTrue :: Test
testEvalStringIfTrue =
  let expected = "Right x"
   in let actual = show (evalString "(if #true x y)")
       in let message = "Evaluation returned an unexpected value."
           in TestCase (assertEqual message expected actual)

testEvalStringIfFalse :: Test
testEvalStringIfFalse =
  let expected = "Right x"
   in let actual = show (evalString "(if #true x y)")
       in let message = "Evaluation returned an unexpected value."
           in TestCase (assertEqual message expected actual)

testExtraArgs :: Test
testExtraArgs =
  let expected = "Left \"Error at ((lambda (x) x) y z) (line 1, column 1):\\n\\tCould not evaluate function - expected 1 arguments, but got 2\""
   in let actual = show (evalString "((lambda (x) x) y z)")
       in let message = "An unexpected error message was returned."
           in TestCase (assertEqual message expected actual)

testInvalidArgs :: Test
testInvalidArgs =
  let expected = "Left \"Error at ((lambda (x) x) y z) (line 1, column 17):\\n\\tCould not evaluate function - expected 1 arguments, but got 2\""
   in let actual = show (evalString "((lambda (x) x) ((lambda (x) x) y z))")
       in let message = "An unexpected error message was returned."
           in TestCase (assertEqual message expected actual)

testAdd :: Test
testAdd =
  let expected = "Right 58"
   in let actual = show (evalString "(+ 100 -42)")
       in let message = "Evaluation returned an unexpected value."
           in TestCase (assertEqual message expected actual)

testSub :: Test
testSub =
  let expected = "Right -3"
   in let actual = show (evalString "(- 7 10)")
       in let message = "Evaluation returned an unexpected value."
           in TestCase (assertEqual message expected actual)

testMul :: Test
testMul =
  let expected = "Right 256"
   in let actual = show (evalString "(* 16 16)")
       in let message = "Evaluation returned an unexpected value."
           in TestCase (assertEqual message expected actual)

testDiv :: Test
testDiv =
  let expected = "Right -3"
   in let actual = show (evalString "(/ -51 13)")
       in let message = "Evaluation returned an unexpected value."
           in TestCase (assertEqual message expected actual)

testAddLeftBool :: Test
testAddLeftBool =
  let expected = "Left \"Error at (+ #true 1) (line 1, column 1):\\n\\tCould not evaluate +, expected left argument to be an integer but got #true\""
   in let actual = show (evalString "(+ #true 1)")
       in let message = "An unexpected error message was returned."
           in TestCase (assertEqual message expected actual)

testSubRightFvar :: Test
testSubRightFvar =
  let expected = "Left \"Error at (- 0 x) (line 1, column 1):\\n\\tCould not evaluate -, expected right argument to be an integer but got x\""
   in let actual = show (evalString "(- 0 x)")
       in let message = "An unexpected error message was returned."
           in TestCase (assertEqual message expected actual)

testMulLeftError :: Test
testMulLeftError =
  let expected = "Left \"Error at ((lambda (x) x) x y) (line 1, column 4):\\n\\tCould not evaluate function - expected 1 arguments, but got 2\""
   in let actual = show (evalString "(* ((lambda (x) x) x y) -1)")
       in let message = "An unexpected error message was returned."
           in TestCase (assertEqual message expected actual)

testDivRightError :: Test
testDivRightError =
  let expected = "Left \"Error at ((lambda (x) x) x y) (line 1, column 6):\\n\\tCould not evaluate function - expected 1 arguments, but got 2\""
   in let actual = show (evalString "(/ 2 ((lambda (x) x) x y))")
       in let message = "An unexpected error message was returned."
           in TestCase (assertEqual message expected actual)

testLt :: Test
testLt =
  let expected = "Right #false"
   in let actual = show (evalString "(< 0 0)")
       in let message = "Evaluation returned an unexpected value."
           in TestCase (assertEqual message expected actual)

testLeq :: Test
testLeq =
  let expected = "Right #true"
   in let actual = show (evalString "(<= 0 0)")
       in let message = "Evaluation returned an unexpected value."
           in TestCase (assertEqual message expected actual)

testEq :: Test
testEq =
  let expected = "Right #true"
   in let actual = show (evalString "(= 0 0)")
       in let message = "Evaluation returned an unexpected value."
           in TestCase (assertEqual message expected actual)

testGeq :: Test
testGeq =
  let expected = "Right #true"
   in let actual = show (evalString "(>= 0 0)")
       in let message = "Evaluation returned an unexpected value."
           in TestCase (assertEqual message expected actual)

testGt :: Test
testGt =
  let expected = "Right #false"
   in let actual = show (evalString "(> 0 0)")
       in let message = "Evaluation returned an unexpected value."
           in TestCase (assertEqual message expected actual)

testNeq :: Test
testNeq =
  let expected = "Right #false"
   in let actual = show (evalString "(/= 0 0)")
       in let message = "Evaluation returned an unexpected value."
           in TestCase (assertEqual message expected actual)

testUnit :: Test
testUnit =
  let expected = "Right ()"
   in let actual = show (evalString "()")
       in let message = "Evaluation returned an unexpected value."
           in TestCase (assertEqual message expected actual)

testLetAdd :: Test
testLetAdd =
  let expected = "Right 3"
   in let actual = show (evalString "(let ((a 1) (b 2)) (+ a b))")
       in let message = "Evaluation returned an unexpected value."
           in TestCase (assertEqual message expected actual)

testLetIfApp :: Test
testLetIfApp =
  let expected = "Right a"
   in let actual = show (evalString "(let ((f (lambda (x y z) (if x y z))) (z #true) (y a) (x b)) (f z y x))")
       in let message = "Evaluation returned an unexpected value."
           in TestCase (assertEqual message expected actual)

testLetShadowing :: Test
testLetShadowing =
  let expected = "Right 1"
   in let actual = show (evalString "(let ((a 0) (a (+ a 1))) a)")
       in let message = "Evaluation returned an unexpected value."
           in TestCase (assertEqual message expected actual)

testLetParseExpectRparenButEOF :: Test
testLetParseExpectRparenButEOF =
  let expected = "Left \"Error at  (line 1, column 15):\\n\\tParsing error: expected ')' but reached end of file\""
   in let actual = show (evalString "(let ((a 0)) a")
       in let message = "An unexpected error message was returned."
           in TestCase (assertEqual message expected actual)

testEvalAnfStringComplexLambda :: Test
testEvalAnfStringComplexLambda =
  let expected = "Right (lambda (y) y)"
   in let actual = show (evalAnfString "((lambda (x) (x x)) ((lambda (f) (f (f (lambda (y) y)))) (lambda (x) x)))")
       in let message = "Evaluation returned an unexpected value."
           in TestCase (assertEqual message expected actual)

tests =
  TestList
    [ TestLabel "testskk" testskk
    , TestLabel "tessti" testi
    , TestLabel "testIfTrue" testIfTrue
    , TestLabel "testIfFalse" testIfFalse
    , TestLabel "testTokenize" testTokenize
    , TestLabel "testEvalStringI" testEvalStringI
    , TestLabel "testEvalStringComplexLambda" testEvalStringComplexLambda
    , TestLabel "testEvalStringIfTrue" testEvalStringIfTrue
    , TestLabel "testEvalStringIfFalse" testEvalStringIfFalse
    , TestLabel "testExtraArgs" testExtraArgs
    , TestLabel "testInvalidArgs" testInvalidArgs
    , TestLabel "testAdd" testAdd
    , TestLabel "testSub" testSub
    , TestLabel "testMul" testMul
    , TestLabel "testDiv" testDiv
    , TestLabel "testLt" testLt
    , TestLabel "testLeq" testLeq
    , TestLabel "testEq" testEq
    , TestLabel "testGeq" testGeq
    , TestLabel "testGt" testGt
    , TestLabel "testNeq" testNeq
    , TestLabel "testAddLeftBool" testAddLeftBool
    , TestLabel "testSubRightFvar" testSubRightFvar
    , TestLabel "testMulLeftError" testMulLeftError
    , TestLabel "testDivRightError" testDivRightError
    , TestLabel "testLetAdd" testLetAdd
    , TestLabel "testLetIfApp" testLetIfApp
    , TestLabel "testLetShadowing" testLetShadowing
    , TestLabel "testLetParseExpectRparenButEOF" testLetParseExpectRparenButEOF
    , TestLabel "testEvalAnfStringComplexLambda" testEvalAnfStringComplexLambda
    ]

main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
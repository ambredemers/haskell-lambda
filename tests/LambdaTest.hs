module Main where
import Lambda
import Test.HUnit
import qualified System.Exit as Exit
import qualified Data.Text as Text
import Data.Char

-- test :: Test
-- test =
--     let expected = expected
--     in let actual = actual
--     in let message = "expected " ++ show expected ++ " but got " ++ show actual
--     in TestCase (assertEqual message expected actual)

testskk :: Test
testskk =
    let expected = Right (tFVar "x") :: Either String Term
    in let actual = eval (Text.pack "") (tApp (tApp (tApp s [k]) [k]) [tFVar "x"]) []
    in let message = "testskk: evaluation returned an unexpected value."
    in TestCase (assertEqual message expected actual)

testi :: Test
testi =
    let expected = Right (tFVar "x") :: Either String Term
    in let actual = eval (Text.pack "") (tApp i [tFVar "x"]) []
    in let message = "testi: evaluation returned an unexpected value."
    in TestCase (assertEqual message expected actual)

testIfTrue :: Test
testIfTrue =
    let expected = Right (tFVar "x") :: Either String Term
    in let actual = eval (Text.pack "") (tIf tTrue (tFVar "x") (tFVar "y")) []
    in let message = "testIfTrue: evaluation returned an unexpected value."
    in TestCase (assertEqual message expected actual)

testIfFalse :: Test
testIfFalse =
    let expected = Right (tFVar "y") :: Either String Term
    in let actual = eval (Text.pack "") (tIf tFalse (tFVar "x") (tFVar "y")) []
    in let message = "testIfFalse: evaluation returned an unexpected value."
    in TestCase (assertEqual message expected actual)

testTokenize :: Test
testTokenize =
    let expected =
            [ Lparen {lpDbg = Dbg {dStart = 0, dEnd = 1}}
            , Lambda {laDbg = Dbg {dStart = 1, dEnd = 7}}
            , Lparen {lpDbg = Dbg {dStart = 8, dEnd = 9}}
            , ToVar {name = Text.pack "left", toVDbg = Dbg {dStart = 9, dEnd = 13}}
            , ToVar {name = Text.pack "right", toVDbg = Dbg {dStart = 14, dEnd = 19}}
            , Rparen {rpDbg = Dbg {dStart = 19, dEnd = 20}}
            , Lparen {lpDbg = Dbg {dStart = 21, dEnd = 22}}
            , ToIf {toIfDbg = Dbg {dStart = 22, dEnd = 24}}
            , ToVar {name = Text.pack "left", toVDbg = Dbg {dStart = 25, dEnd = 29}}
            , Lparen {lpDbg = Dbg {dStart = 30, dEnd = 31}}
            , ToIf {toIfDbg = Dbg {dStart = 31, dEnd = 33}}
            , ToVar {name = Text.pack "right", toVDbg = Dbg {dStart = 34, dEnd = 39}}
            , ToTrue {toTDbg = Dbg {dStart = 40, dEnd = 45}}
            , ToFalse {toFDbg = Dbg {dStart = 46, dEnd = 52}}
            , Rparen {rpDbg = Dbg {dStart = 52, dEnd = 53}}
            , ToFalse {toFDbg = Dbg {dStart = 54, dEnd = 60}}
            , Rparen {rpDbg = Dbg {dStart = 60, dEnd = 61}}
            , Rparen {rpDbg = Dbg {dStart = 61, dEnd = 62}}]
    in let actual = tokenize (Text.pack "(lambda (left right) (if left (if right #true #false) #false))")
    in let message = "testTokenize: evaluation returned an unexpected value."
    in TestCase (assertEqual message expected actual)

testParseTerm :: Test
testParseTerm =
    let expected =
            (Right (PApp
                { paFun = PLambda
                    { plVars =
                        [ PVar {pvName = Text.pack "left", pvDbg = Dbg {dStart = 10, dEnd = 14}}
                        , PVar {pvName = Text.pack "right", pvDbg = Dbg {dStart = 15, dEnd = 20}}]
                    , plBody = PIf
                        { piCond = PVar {pvName = Text.pack "left", pvDbg = Dbg {dStart = 26, dEnd = 30}}
                        , piCnsq = PIf
                            { piCond = PVar {pvName = Text.pack "right", pvDbg = Dbg {dStart = 35, dEnd = 40}}
                            , piCnsq = PBool {pbBool = True, pbDbg = Dbg {dStart = 41, dEnd = 46}}
                            , piAlt = PBool {pbBool = False, pbDbg = Dbg {dStart = 47, dEnd = 53}}
                            , piDbg = Dbg {dStart = 31, dEnd = 54}}
                        , piAlt = PBool {pbBool = False, pbDbg = Dbg {dStart = 55, dEnd = 61}}
                        , piDbg = Dbg {dStart = 22, dEnd = 62}}, plDbg = Dbg {dStart = 1, dEnd = 63}}
                , paArgs =
                    [ PBool {pbBool = True, pbDbg = Dbg {dStart = 64, dEnd = 69}}
                    , PBool {pbBool = True, pbDbg = Dbg {dStart = 70, dEnd = 75}}]
                , paDbg = Dbg {dStart = 0, dEnd = 76}})
            , []) :: (Either String PTerm, [Token])
    in let input = Text.pack "((lambda (left right) (if left (if right #true #false) #false)) #true #true)"
    in let actual = parseTerm input (tokenize input)
    in let message = "testParseTerm: evaluation returned an unexpected value."
    in TestCase (assertEqual message expected actual)

testEvalStringI :: Test
testEvalStringI =
    let expected = "Right (lambda (x) x)"
    in let actual = show (evalString "(lambda (x) x)")
    in let message = "testEvalStringI: evaluation returned an unexpected value."
    in TestCase (assertEqual message expected actual)

testEvalStringComplexLambda :: Test
testEvalStringComplexLambda =
    let expected = "Right (lambda (y) y)"
    in let actual = show (evalString "((lambda (x) (x x)) ((lambda (f) (f (f (lambda (y) y)))) (lambda (x) x)))")
    in let message = "testEvalStringComplexLambda: evaluation returned an unexpected value."
    in TestCase (assertEqual message expected actual)

testEvalStringIfTrue :: Test
testEvalStringIfTrue =
    let expected = "Right x"
    in let actual = show (evalString "(if #true x y)")
    in let message = "testEvalStringIfTrue: evaluation returned an unexpected value."
    in TestCase (assertEqual message expected actual)

testEvalStringIfFalse :: Test
testEvalStringIfFalse =
    let expected = "Right x"
    in let actual = show (evalString "(if #true x y)")
    in let message = "testEvalStringIfFalse: evaluation returned an unexpected value."
    in TestCase (assertEqual message expected actual)

testExtraArgs :: Test
testExtraArgs =
    let expected = "Left \"Error at ((lambda (x) x) y z) (line 1, column 1):\\n\\tCould not evaluate function - expected 1 arguments, but got 2\""
    in let actual = show (evalString "((lambda (x) x) y z)")
    in let message = "testExtraArgs: an unexpected error message was returned."
    in TestCase (assertEqual message expected actual)

testInvalidArgs :: Test
testInvalidArgs =
    let expected = "Left \"Error at ((lambda (x) x) y z) (line 1, column 17):\\n\\tCould not evaluate function - expected 1 arguments, but got 2\""
    in let actual = show (evalString "((lambda (x) x) ((lambda (x) x) y z))")
    in let message = "testInvalidArgs: an unexpected error message was returned."
    in TestCase (assertEqual message expected actual)

testAdd :: Test
testAdd =
    let expected = "Right 58"
    in let actual = show (evalString "(+ 100 -42)")
    in let message = "testAdd: evaluation returned an unexpected value."
    in TestCase (assertEqual message expected actual)

testSub :: Test
testSub =
    let expected = "Right -3"
    in let actual = show (evalString "(- 7 10)")
    in let message = "testSub: evaluation returned an unexpected value."
    in TestCase (assertEqual message expected actual)

testMul :: Test
testMul =
    let expected = "Right 256"
    in let actual = show (evalString "(* 16 16)")
    in let message = "testMul: evaluation returned an unexpected value."
    in TestCase (assertEqual message expected actual)

testDiv :: Test
testDiv =
    let expected = "Right -3"
    in let actual = show (evalString "(/ -51 13)")
    in let message = "testDiv: evaluation returned an unexpected value."
    in TestCase (assertEqual message expected actual)

testAddLeftBool :: Test
testAddLeftBool =
    let expected = "Left \"Error at (+ #true 1) (line 1, column 1):\\n\\tCould not evaluate +, expected left argument to be an integer but got #true\""
    in let actual = show (evalString "(+ #true 1)")
    in let message = "testAddLeftBool: an unexpected error message was returned."
    in TestCase (assertEqual message expected actual)

testSubRightFvar :: Test
testSubRightFvar =
    let expected = "Left \"Error at (- 0 x) (line 1, column 1):\\n\\tCould not evaluate -, expected right argument to be an integer but got x\""
    in let actual = show (evalString "(- 0 x)")
    in let message = "testSubRightFvar: an unexpected error message was returned."
    in TestCase (assertEqual message expected actual)

testMulLeftError :: Test
testMulLeftError =
    let expected = "Left \"Error at ((lambda (x) x) x y) (line 1, column 4):\\n\\tCould not evaluate function - expected 1 arguments, but got 2\""
    in let actual = show (evalString "(* ((lambda (x) x) x y) -1)")
    in let message = "testMulLeftError: an unexpected error message was returned."
    in TestCase (assertEqual message expected actual)

testDivRightError :: Test
testDivRightError =
    let expected = "Left \"Error at ((lambda (x) x) x y) (line 1, column 6):\\n\\tCould not evaluate function - expected 1 arguments, but got 2\""
    in let actual = show (evalString "(/ 2 ((lambda (x) x) x y))")
    in let message = "testDivRightError: an unexpected error message was returned."
    in TestCase (assertEqual message expected actual)

tests = TestList
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
    , TestLabel "testAddLeftBool" testAddLeftBool
    , TestLabel "testSubRightFvar" testSubRightFvar
    , TestLabel "testMulLeftError" testMulLeftError
    , TestLabel "testDivRightError" testDivRightError ]

main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
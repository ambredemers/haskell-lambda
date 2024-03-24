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
    let expected = fvar "x"
    in let actual = eval (app (app (app s [k]) [k]) [fvar "x"]) []
    in let message = "expected " ++ show expected ++ " but got " ++ show actual
    in TestCase (assertEqual message expected actual)

testi :: Test
testi =
    let expected = fvar "x"
    in let actual = eval (app i [fvar "x"]) []
    in let message = "expected " ++ show expected ++ " but got " ++ show actual
    in TestCase (assertEqual message expected actual)

testIfTrue :: Test
testIfTrue =
    let expected = fvar "x"
    in let actual = eval (tif ttrue (fvar "x") (fvar "y")) []
    in let message = "expected " ++ show expected ++ " but got " ++ show actual
    in TestCase (assertEqual message expected actual)

testIfFalse :: Test
testIfFalse =
    let expected = fvar "y"
    in let actual = eval (tif tfalse (fvar "x") (fvar "y")) []
    in let message = "expected " ++ show expected ++ " but got " ++ show actual
    in TestCase (assertEqual message expected actual)

testTokenize :: Test
testTokenize =
    let source = Text.pack "(lambda (left right) (if left (if right #true #false) #false))"
    in let expected =
            [ Lparen {lpDbg = Dbg {dSource = source, dStart = 0, dLength = 1}}
            , Lambda {laDbg = Dbg {dSource = source, dStart = 1, dLength = 6}}
            , Lparen {lpDbg = Dbg {dSource = source, dStart = 8, dLength = 1}}
            , ToVar {name = Text.pack "left", toVDbg = Dbg {dSource = source, dStart = 9, dLength = 4}}
            , ToVar {name = Text.pack "right", toVDbg = Dbg {dSource = source, dStart = 14, dLength = 5}}
            , Rparen {rpDbg = Dbg {dSource = source, dStart = 19, dLength = 1}}
            , Lparen {lpDbg = Dbg {dSource = source, dStart = 21, dLength = 1}}
            , ToIf {toIfDbg = Dbg {dSource = source, dStart = 22, dLength = 2}}
            , ToVar {name = Text.pack "left", toVDbg = Dbg {dSource = source, dStart = 25, dLength = 4}}
            , Lparen {lpDbg = Dbg {dSource = source, dStart = 30, dLength = 1}}
            , ToIf {toIfDbg = Dbg {dSource = source, dStart = 31, dLength = 2}}
            , ToVar {name = Text.pack "right", toVDbg = Dbg {dSource = source, dStart = 34, dLength = 5}}
            , ToTrue {toTDbg = Dbg {dSource = source, dStart = 40, dLength = 5}}
            , ToFalse {toFDbg = Dbg {dSource = source, dStart = 46, dLength = 6}}
            , Rparen {rpDbg = Dbg {dSource = source, dStart = 52, dLength = 1}}
            , ToFalse {toFDbg = Dbg {dSource = source, dStart = 54, dLength = 6}}
            , Rparen {rpDbg = Dbg {dSource = source, dStart = 60, dLength = 1}}
            , Rparen {rpDbg = Dbg {dSource = source, dStart = 61, dLength = 1}}]
    in let actual = tokenize source
    in let message = "expected " ++ show expected ++ " but got " ++ show actual
    in TestCase (assertEqual message expected actual)

tests = TestList
    [
        TestLabel "testskk" testskk,
        TestLabel "tessti" testi,
        TestLabel "testIfTrue" testIfTrue,
        TestLabel "testIfFalse" testIfFalse,
        TestLabel "testTokenize" testTokenize
    ]

main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
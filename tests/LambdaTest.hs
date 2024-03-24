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
    in let message = "expected " ++ show expected ++ " but got " ++ show actual
    in TestCase (assertEqual message expected actual)

tests = TestList
    [ TestLabel "testskk" testskk
    , TestLabel "tessti" testi
    , TestLabel "testIfTrue" testIfTrue
    , TestLabel "testIfFalse" testIfFalse
    , TestLabel "testTokenize" testTokenize]

main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
module Main where
import Lambda
import Test.HUnit
import qualified System.Exit as Exit
import qualified Data.Text as Text
import Data.Char

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

testBreakLength :: Test
testBreakLength =
    let expected = (Text.pack "abcde", 5, Text.pack " fgh")
    in let actual = breakLength isSpace (Text.pack "abcde fgh")
    in let message = "expected " ++ show expected ++ " but got " ++ show actual
    in TestCase (assertEqual message expected actual)

testDropWhileLength :: Test
testDropWhileLength =
    let expected = (Text.pack "x    ", 4)
    in let actual = dropWhileLength isSpace (Text.pack "    x    ")
    in let message = "expected " ++ show expected ++ " but got " ++ show actual
    in TestCase (assertEqual message expected actual)

tests = TestList
    [
        TestLabel "testskk" testskk,
        TestLabel "tessti" testi,
        TestLabel "testIfTrue" testIfTrue,
        TestLabel "testIfFalse" testIfFalse,
        TestLabel "testBreakLength" testBreakLength,
        TestLabel "testDropWhileLength" testDropWhileLength
    ]

main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
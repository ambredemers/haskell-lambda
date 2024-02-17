module Main where
import Lambda
import Test.HUnit
import qualified System.Exit as Exit

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

tests = TestList
    [
        TestLabel "testskk" testskk,
        TestLabel "tessti" testi
    ]

main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
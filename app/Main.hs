module Main where
import Control.Monad
import Interpreter
import System.IO

main :: IO ()
main = do
    putStr "> "
    hFlush stdout
    input <- getLine
    unless (input == "(quit)") $ do
        putStrLn (either id show (evalString input))
        main
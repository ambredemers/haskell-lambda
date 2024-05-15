module Main where

import Control.Monad
import InterpretAnf
import InterpretTerm
import System.IO

main :: IO ()
main = do
  putStr "> "
  hFlush stdout
  input <- getLine
  unless (input == "(quit)") $ do
    putStrLn $ either id show (evalString input)
    putStrLn $ either id show (evalAnfString input)
    main
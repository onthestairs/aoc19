module Main (main) where

import Solve.Day7

main :: IO ()
main = do
  x <- parseAndSolve2
  putTextLn (show x)

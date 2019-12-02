module Solve.Day1 where

import Parsing
import Text.Megaparsec
import Text.Megaparsec.Char

type Input = [Int]

runParseInput :: IO (Maybe Input)
runParseInput = runParseFile "./data/1.input" parseInput

parseInput :: Parser [Int]
parseInput = sepBy1 parseInteger newline <* eof

parseAndSolve1 :: IO (Maybe Int)
parseAndSolve1 = do
  input <- runParseInput
  pure $ solve1 <$> input

requiredFuel :: Int -> Int
requiredFuel n = (n `div` 3) - 2

solve1 ms = sum $ map requiredFuel ms

requiredFuel2 :: Int -> Int
requiredFuel2 n =
  let baseFule = (n `div` 3) - 2
   in if baseFule <= 0 then 0 else baseFule + requiredFuel2 baseFule

parseAndSolve2 :: IO (Maybe Int)
parseAndSolve2 = do
  input <- runParseInput
  pure $ solve2 <$> input

solve2 ms = sum $ map requiredFuel2 ms

module Solve.Day1 where

import Data.Functor.Foldable
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
requiredFuel2 = hylo alg coalg
  where
    alg (Cons a b) = a + b
    alg Nil = 0
    coalg n = let fuel = (n `div` 3) - 2 in if fuel <= 0 then Nil else Cons fuel fuel

parseAndSolve2 :: IO (Maybe Int)
parseAndSolve2 = do
  input <- runParseInput
  pure $ solve2 <$> input

solve2 ms = sum $ map requiredFuel2 ms

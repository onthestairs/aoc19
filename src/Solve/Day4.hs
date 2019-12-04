module Solve.Day4 where

import Parsing
import Text.Megaparsec
import Text.Megaparsec.Char

type Input = (Int, Int)

runParseInput :: IO (Maybe Input)
runParseInput = runParseFile "./data/4.input" parseInput

parseInput :: Parser Input
parseInput = (,) <$> (parseInteger <* char '-') <*> parseInteger

parseAndSolve1 :: IO (Maybe Int)
parseAndSolve1 = do
  input <- runParseInput
  pure $ solve1 <$> input

toDigits :: Int -> [Int]
toDigits n
  | n >= 10 = toDigits (n `div` 10) ++ [(n `mod` 10)]
  | otherwise = [n]

getPairs :: [a] -> [(a, a)]
getPairs xs = case nonEmpty xs of
  Just (x :| xs') -> zip xs xs'
  Nothing -> error "nooo"

getWindow4 :: [a] -> [(Maybe a, Maybe a, Maybe a, Maybe a)]
getWindow4 xs = getQuads $ [Nothing] <> (map Just xs) <> [Nothing]

zip4 ws xs ys zs = map (\((w, x, y), z) -> (w, x, y, z)) (zip (zip3 ws xs ys) zs)

getQuads xs = case nonEmpty xs of
  Just (_ :| xs') -> case nonEmpty xs' of
    Just (_' :| xs'') -> case nonEmpty xs'' of
      Just (_ :| xs''') -> zip4 xs xs' xs'' xs'''
      Nothing -> []
    Nothing -> []
  Nothing -> []

isIncreasing :: [Int] -> Bool
isIncreasing ds = all (\(d1, d2) -> d2 >= d1) (getPairs ds)

containsPair :: [Int] -> Bool
containsPair ds = any (\(d1, d2) -> d1 == d2) (getPairs ds)

solve1 :: (Int, Int) -> Int
solve1 (n1, n2) = length $ filter isValid [n1 .. n2]
  where
    isValid n = let digits = toDigits n in isIncreasing digits && containsPair digits

parseAndSolve2 :: IO (Maybe Int)
parseAndSolve2 = do
  input <- runParseInput
  pure $ solve2 <$> input

containsPairAndNotTriple :: [Int] -> Bool
containsPairAndNotTriple ds = any (\(d1, d2, d3, d4) -> d2 == d3 && d1 /= d2 && d3 /= d4) (getWindow4 ds)

solve2 :: (Int, Int) -> Int
solve2 (n1, n2) = length $ filter isValid [n1 .. n2]
  where
    isValid n = let digits = toDigits n in isIncreasing digits && containsPairAndNotTriple digits

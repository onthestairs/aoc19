module Solve.Day3 where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Parsing
import Relude.Extra.Foldable1
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (Down, Left, Right)

data Move = Left Int | Right Int | Up Int | Down Int deriving (Show)

type Input = ([Move], [Move])

runParseInput :: IO (Maybe Input)
runParseInput = runParseFile "./data/3.input" parseInput

parseMove :: Parser Move
parseMove = (Left <$> (char 'L' *> parseInteger)) <|> (Right <$> (char 'R' *> parseInteger)) <|> (Down <$> (char 'D' *> parseInteger)) <|> (Up <$> (char 'U' *> parseInteger))

parseMoves :: Parser [Move]
parseMoves = sepBy1 parseMove (char ',')

parseInput :: Parser Input
parseInput = do
  moves1 <- parseMoves
  newline
  moves2 <- parseMoves
  pure $ (moves1, moves2)

parseAndSolve1 :: IO (Maybe Int)
parseAndSolve1 = do
  input <- runParseInput
  pure $ do
    i <- input
    solve1 i

findPath [] _ = Set.empty
findPath ((Left n) : ms) (x, y) = Set.fromList [(x - z, y) | z <- [1 .. n]] `Set.union` findPath ms (x - n, y)
findPath ((Right n) : ms) (x, y) = Set.fromList [(x + z, y) | z <- [1 .. n]] `Set.union` findPath ms (x + n, y)
findPath ((Up n) : ms) (x, y) = Set.fromList [(x, y - z) | z <- [1 .. n]] `Set.union` findPath ms (x, y - n)
findPath ((Down n) : ms) (x, y) = Set.fromList [(x, y + z) | z <- [1 .. n]] `Set.union` findPath ms (x, y + n)

solve1 :: Input -> Maybe Int
solve1 (ms, ms') =
  let path1 = findPath ms (0, 0)
      path2 = findPath ms' (0, 0)
      crossers = Set.intersection path1 path2
   in viaNonEmpty minimum1 [abs x + abs y | (x, y) <- toList crossers]

parseAndSolve2 :: IO (Maybe Int)
parseAndSolve2 = do
  input <- runParseInput
  pure $ do
    i <- input
    solve2 i

makeVisitedMap [] _ _ = Map.empty
makeVisitedMap ((Left n) : ms) c (x, y) = Map.fromList [((x - z, y), c + z) | z <- [1 .. n]] `Map.union` makeVisitedMap ms (c + n) (x - n, y)
makeVisitedMap ((Right n) : ms) c (x, y) = Map.fromList [((x + z, y), c + z) | z <- [1 .. n]] `Map.union` makeVisitedMap ms (c + n) (x + n, y)
makeVisitedMap ((Up n) : ms) c (x, y) = Map.fromList [((x, y - z), c + z) | z <- [1 .. n]] `Map.union` makeVisitedMap ms (c + n) (x, y - n)
makeVisitedMap ((Down n) : ms) c (x, y) = Map.fromList [((x, y + z), c + z) | z <- [1 .. n]] `Map.union` makeVisitedMap ms (c + n) (x, y + n)

solve2 (ms, ms') =
  let map1 = makeVisitedMap ms 0 (0, 0)
      map2 = makeVisitedMap ms' 0 (0, 0)
      crossers = Map.intersectionWith (+) map1 map2
   in viaNonEmpty minimum1 (Map.elems crossers)

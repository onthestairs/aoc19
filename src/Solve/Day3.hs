module Solve.Day3 where

import Data.Functor.Foldable
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

move (Left n) (x, y) = (x - n, y)
move (Right n) (x, y) = (x + n, y)
move (Up n) (x, y) = (x, y - n)
move (Down n) (x, y) = (x, y + n)

pathLength (Left n) = n
pathLength (Right n) = n
pathLength (Up n) = n
pathLength (Down n) = n

fst3 (x, y, z) = x

runPath :: Monoid m => ((Move, Int, (Int, Int)) -> m) -> [Move] -> m
runPath f ps = fst3 $ cata alg (reverse ps)
  where
    alg Nil = (mempty, 0, (0, 0))
    alg (Cons p (m, c, pos)) = (m <> f (p, c, pos), c + (pathLength p), move p pos)

makePath (Left n) _ (x, y) = [(x - z, y) | z <- [1 .. n]]
makePath (Right n) _ (x, y) = [(x + z, y) | z <- [1 .. n]]
makePath (Up n) _ (x, y) = [(x, y - z) | z <- [1 .. n]]
makePath (Down n) _ (x, y) = [(x, y + z) | z <- [1 .. n]]

findPath = runPath (\(p, c, pos) -> Set.fromList $ makePath p c pos)

solve1 :: Input -> Maybe Int
solve1 (ms, ms') =
  let path1 = findPath ms
      path2 = findPath ms'
      crossers = Set.intersection path1 path2
   in viaNonEmpty minimum1 [abs x + abs y | (x, y) <- toList crossers]

parseAndSolve2 :: IO (Maybe Int)
parseAndSolve2 = do
  input <- runParseInput
  pure $ do
    i <- input
    solve2 i

makeVisitedAtMap (Left n) c (x, y) = Map.fromList [((x - z, y), c + z) | z <- [1 .. n]]
makeVisitedAtMap (Right n) c (x, y) = Map.fromList [((x + z, y), c + z) | z <- [1 .. n]]
makeVisitedAtMap (Up n) c (x, y) = Map.fromList [((x, y - z), c + z) | z <- [1 .. n]]
makeVisitedAtMap (Down n) c (x, y) = Map.fromList [((x, y + z), c + z) | z <- [1 .. n]]

makeVisitedMap = runPath (\(p, c, pos) -> makeVisitedAtMap p c pos)

solve2 (ms, ms') =
  let map1 = makeVisitedMap ms
      map2 = makeVisitedMap ms'
      crossers = Map.intersectionWith (+) map1 map2
   in viaNonEmpty minimum1 (Map.elems crossers)

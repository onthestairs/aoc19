module Solve.Day1 where

import Data.Functor.Foldable
import qualified Data.Set as Set
import GHC.Float (atan2)
import Linear.V2
import Parsing
import Relude.Extra.Foldable1
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (many)

type Point = V2 Int

type Input = Set.Set Point

data PointInSpace = Asteroid | Vacuum deriving (Eq, Show)

runParseInput :: IO (Maybe Input)
runParseInput = runParseFile "./data/10.input" parseInput

parseCell :: Parser PointInSpace
parseCell = do
  (char '#' *> (pure $ Asteroid)) <|> (char '.' *> (pure $ Vacuum))

parseRow :: Parser [PointInSpace]
parseRow = many parseCell

rowToAsteroidPoints y cs = cata alg (zip [0 ..] cs)
  where
    alg Nil = Set.empty
    alg (Cons (x, c) as) = if c == Asteroid then Set.singleton (V2 x y) <> as else as

cellsToAsteroidPoints :: [[PointInSpace]] -> Set.Set Point
cellsToAsteroidPoints cells = cata alg (zip [0 ..] cells)
  where
    alg Nil = Set.empty
    alg (Cons (y, cs) as) = rowToAsteroidPoints y cs <> as

parseInput :: Parser Input
parseInput = do
  rows <- sepBy parseRow newline <* eof
  pure $ cellsToAsteroidPoints rows

parseAndSolve1 :: IO (Maybe Int)
parseAndSolve1 = do
  input <- runParseInput
  pure $ solve1 <$> input

-- requiredFuel :: Int -> Int
-- requiredFuel n = (n `div` 3) - 2

pointsBetween p1@(V2 x1 y1) p2@(V2 x2 y2) = takeWhile (/= p2) $ drop 1 $ iterate (+ step) p1
  where
    d@(V2 xD yD) = (p2 - p1)
    factor = gcd xD yD
    step = traceShowId $ fmap (`div` factor) d

findVisible p1 as = Set.filter (\p2 -> (p1 /= p2) && (not $ any (flip Set.member as) $ pointsBetween p1 p2)) as

numberVisible p as = length $ findVisible p as

solve1 as = Set.findMax $ Set.map (\p -> numberVisible p as) as

-- parseAndSolve2 :: IO (Maybe Int)
parseAndSolve2 = do
  input <- runParseInput
  pure $ do
    i <- input
    solve2 <$> input

destroyedPointsFrom :: Point -> Set.Set Point -> [Point]
destroyedPointsFrom p1 = hylo alg coalg
  where
    coalg :: Set.Set Point -> ListF [Point] (Set.Set Point)
    coalg s
      | s == Set.empty = Nil
      | otherwise = let visiblePoints = findVisible p1 s in Cons (sortOn (toAngle p1) (toList visiblePoints)) (Set.difference s visiblePoints)
    alg Nil = []
    alg (Cons xs ys) = xs <> ys

toAngle :: Point -> Point -> Float
toAngle p1 p2 =
  let (V2 o a) = p2 - p1
   in atan2 (- fromIntegral o) (fromIntegral a)

findCentre :: Set.Set Point -> Maybe Point
findCentre as = viaNonEmpty head $ sortOn (\p -> Down $ numberVisible p as) (toList as)

solve2 as = do
  centre <- findCentre as
  let destroyedPoints = destroyedPointsFrom centre as
  (V2 x y) <- viaNonEmpty head $ drop 199 $ destroyedPoints
  pure $ (100 * x) + y

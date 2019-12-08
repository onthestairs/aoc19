module Solve.Day8 where

import Data.Functor.Foldable
import Parsing
import Text.Megaparsec hiding (chunk, count)
import Text.Megaparsec.Char
import qualified Text.Show
import Prelude hiding (many)

type Input = [Int]

-- runParseInput :: IO (Maybe Input)
runParseInput = runParseFile "./data/8.input" parseInput

-- parseInput :: Parser [Int]
parseInput = many digitChar <* eof

-- parseAndSolve1 :: IO (Maybe Int)
parseAndSolve1 = do
  input <- runParseInput
  pure $ solve1 <$> input

chunk :: Int -> [a] -> [[a]]
chunk size = para alg
  where
    alg Nil = []
    alg (Cons x (xs, chunks)) = if (length xs + 1) `mod` size == 0 then (x : take (size -1) xs) : chunks else chunks

toLayers :: Int -> Int -> [a] -> [[a]]
toLayers width height ns = chunk (width * height) ns

count x = length . filter (== x)

solve1 ns = do
  layer <- viaNonEmpty head $ sortOn (count '0') $ toLayers 25 6 ns
  let ones = count '1' layer
  let twos = count '2' layer
  pure $ ones * twos

-- parseAndSolve2 :: IO (Maybe Int)
parseAndSolve2 = do
  input <- runParseInput
  output <- pure $ do
    i <- input
    pure $ solve2 i
  putTextLn $ fromMaybe ("fail :(" :: Text) output

data Pixel = Transparent | White | Black

instance Semigroup Pixel where
  Transparent <> p = p
  p <> _ = p

instance Monoid Pixel where
  mempty = Transparent

instance Show Pixel where
  show Transparent = " "
  show White = "#"
  show Black = " "

toPixel '0' = Black
toPixel '1' = White
toPixel '2' = Transparent

zipCollapse :: Monoid a => [[a]] -> [a]
zipCollapse = map mconcat . transpose

solve2 ns =
  let layers = toLayers 25 6 $ map toPixel ns
      finalImage = zipCollapse layers
   in unlines $ (map (foldMap show)) (chunk 25 finalImage)

testImage = (\finalImage -> unlines $ (map (foldMap show)) (chunk 2 finalImage)) $ map mconcat . transpose $ toLayers 2 2 $ map toPixel "0222112222120000"

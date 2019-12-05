module Solve.Day2 where

import Control.Lens
import qualified Data.Map.Strict as Map
import Intcode
import Parsing
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

-- data IntCode = Add | Multiply | Number Int | End deriving (Show)
-- type IntCode = Int

runParseInput :: IO (Maybe (Map.Map Int IntCode))
runParseInput = runParseFile "./data/2.input" parseInput

runParseInputTest :: IO (Maybe (Map.Map Int IntCode))
runParseInputTest = runParseFile "./data/2.input.test" parseInput

parseInput :: Parser (Map.Map Int IntCode)
parseInput = (Map.fromList . zip [0 ..]) <$> sepBy1 parseInteger (char ',') <* eof

-- solve1
parseAndSolve1 :: IO (Maybe IntCode)
parseAndSolve1 = do
  input <- runParseInput
  pure $ do
    i <- input
    output <- solve1 i
    pure output

insertNounAndVerb :: (Map.Map Int IntCode) -> Int -> Int -> (Map.Map Int IntCode)
insertNounAndVerb m noun verb = Map.insert 2 verb (Map.insert 1 noun m)

runTofinalValue :: (Map.Map Int IntCode) -> Int -> Int -> Maybe Int
runTofinalValue ops noun verb = Map.lookup 0 $ view values $ runIntCode [] (insertNounAndVerb ops noun verb)

solve1 ops = runTofinalValue ops 12 2

findInputs ops = filter (\(noun, verb) -> runTofinalValue ops noun verb == Just 19690720) [(noun, verb) | noun <- [0 .. 99], verb <- [0 .. 99]]

--solve2

parseAndSolve2 = do
  input <- runParseInput
  pure $ do
    i <- input
    solve2 i

solve2 ops = viaNonEmpty head $ map (\(n1, n2) -> (100 * n1) + n2) (findInputs ops)

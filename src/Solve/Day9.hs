module Solve.Day9 where

import Control.Lens
import qualified Data.Map.Strict as Map
import Intcode
import Parsing
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

runParseInput :: IO (Maybe (Map.Map Integer IntCode))
runParseInput = runParseFile "./data/9.input" parseInput

runParseInputTest :: IO (Maybe (Map.Map Integer IntCode))
runParseInputTest = runParseFile "./data/9.input.test" parseInput

parseInput :: Parser (Map.Map Integer IntCode)
parseInput = (Map.fromList . zip [0 ..]) <$> sepBy1 parseInteger (char ',') <* eof

-- solve1
-- parseAndSolve1 :: IO (Maybe IntCode)
parseAndSolve1 = do
  input <- runParseInput
  pure $ do
    i <- input
    solve1 i

-- runTofinalValue :: (Map.Map Integer IntCode) -> Maybe Int
runAndGetOutputs inputs ops = view outputs $ runIntCode inputs ops

solve1 ops = viaNonEmpty head $ runAndGetOutputs [1] ops

parseAndSolve2 = do
  input <- runParseInput
  pure $ do
    i <- input
    solve2 i

solve2 ops = viaNonEmpty head $ runAndGetOutputs [2] ops

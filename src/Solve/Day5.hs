module Solve.Day2 where

import qualified Data.Map.Strict as Map
import Intcode
import Parsing
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

runParseInput :: IO (Maybe (Map.Map Int IntCode))
runParseInput = runParseFile "./data/5.input" parseInput

runParseInputTest :: IO (Maybe (Map.Map Int IntCode))
runParseInputTest = runParseFile "./data/5.input.test" parseInput

parseInput :: Parser (Map.Map Int IntCode)
parseInput = (Map.fromList . zip [0 ..]) <$> sepBy1 parseInteger (char ',') <* eof

-- parseAndSolve1 :: IO (Maybe IntCode)
parseAndSolve1 = do
  input <- runParseInput
  pure $ do
    i <- input
    pure $ solve1 i

-- output <- solve1 i
-- pure output

runIntCode is ops = execState (runTilEnd 0) Memory {_inputs = is, _values = ops, _outputs = []}

solve1 ops = runIntCode [1] ops

parseAndSolve2 = do
  input <- runParseInput
  pure $ do
    i <- input
    pure $ solve2 i

solve2 ops = runIntCode [5] ops

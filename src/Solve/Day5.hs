module Solve.Day2 where

import Control.Lens
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

parseAndSolve1 = do
  input <- runParseInput
  pure $ do
    i <- input
    solve1 i

-- runIntCode is ops = execState run (Memory {_position = 0, _inputs = is, _values = ops, _outputs = []})

solve1 ops = viaNonEmpty head $ view outputs $ runIntCode [1] ops

parseAndSolve2 = do
  input <- runParseInput
  pure $ do
    i <- input
    solve2 i

solve2 ops = viaNonEmpty head $ view outputs $ runIntCode [5] ops

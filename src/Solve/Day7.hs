module Solve.Day7 where

import Control.Lens
import Control.Monad.State.Strict (execState)
import Data.List (permutations)
import qualified Data.Map.Strict as Map
import Intcode
import Parsing
import Relude.Extra.Foldable1
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Prelude hiding (execState)

runParseInput :: IO (Maybe (Map.Map Int IntCode))
runParseInput = runParseFile "./data/7.input" parseInput

runParseInputTest :: IO (Maybe (Map.Map Int IntCode))
runParseInputTest = runParseFile "./data/7.input.test" parseInput

parseInput :: Parser (Map.Map Int IntCode)
parseInput = (Map.fromList . zip [0 ..]) <$> sepBy1 parseInteger (char ',') <* eof

parseAndSolve1 = do
  input <- runParseInput
  pure $ do
    i <- input
    solve1 i

-- runIntCode is ops = execState run (Memory {_position = 0, _inputs = is, _values = ops, _outputs = []})

runMany ops pss = go pss 0
  where
    go [] input = input
    go (ps : pss') input = case viaNonEmpty head $ view outputs $ runIntCode [ps, input] ops of
      Just output -> go pss' output
      Nothing -> error "no output!"

solve1 ops = viaNonEmpty maximum1 $ map (runMany ops) (permutations [0 .. 4])

-- runMutual :: Memory -> [Int] -> Maybe Int
runMutual m [p1, p2, p3, p4, p5] =
  -- let o1 = view outputs . execState run $ mem & inputs .~ (ps1 : 0 : o5)
  --     o2 = view outputs . execState run $ mem & inputs .~ (ps2 : o1)
  --     o3 = view outputs . execState run $ mem & inputs .~ (ps3 : o2)
  --     o4 = view outputs . execState run $ mem & inputs .~ (ps4 : o3)
  --     o5 = view outputs . execState run $ mem & inputs .~ (ps5 : o4)
  let r1 = view outputs . execState run $ m & inputs .~ p1 : 0 : r5
      r2 = view outputs . execState run $ m & inputs .~ p2 : r1
      r3 = view outputs . execState run $ m & inputs .~ p3 : r2
      r4 = view outputs . execState run $ m & inputs .~ p4 : r3
      r5 = view outputs . execState run $ m & inputs .~ p5 : r4
   in r5

parseAndSolve2 = do
  input <- runParseInputTest
  pure $ do
    i <- input
    pure $ solve2 i

solve2 ops = viaNonEmpty maximum1 $ map (runMutual (makeInitialMemory ops)) $ take 1 (permutations [5 .. 9])

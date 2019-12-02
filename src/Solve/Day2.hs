module Solve.Day2 where

import qualified Data.Map.Strict as Map
import Parsing
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

-- data IntCode = Add | Multiply | Number Int | End deriving (Show)
type IntCode = Int

runParseInput :: IO (Maybe (Map.Map Int IntCode))
runParseInput = runParseFile "./data/2.input" parseInput

runParseInputTest :: IO (Maybe (Map.Map Int IntCode))
runParseInputTest = runParseFile "./data/2.input.test" parseInput

parseInput :: Parser (Map.Map Int IntCode)
parseInput = (Map.fromList . zip [0 ..]) <$> sepBy1 parseInteger (char ',') <* eof

parseAndSolve1 :: IO (Maybe IntCode)
parseAndSolve1 = do
  input <- runParseInput
  pure $ do
    i <- input
    output <- solve1 i
    pure output

runIntCode input1 input2 ops =
  let finalState = execState (initialReplacements input1 input2 *> runTilEnd 0) ops
   in Map.lookup 0 finalState

solve1 ops = runIntCode 12 2 ops

initialReplacements input1 input2 = do
  setAtPos 1 input1
  setAtPos 2 input2

getAtPos :: Int -> State (Map.Map Int IntCode) IntCode
getAtPos pos = do
  s <- get
  let maybeIntCode = Map.lookup pos s
  pure $ case maybeIntCode of
    Just intCode -> intCode
    Nothing -> error "noooo"

setAtPos :: Int -> IntCode -> State (Map.Map Int IntCode) ()
setAtPos pos intCode = do
  modify (\s -> Map.insert pos intCode s)
  pure ()

fAtPos pos f = do
  pointer1 <- getAtPos (pos + 1)
  pointer2 <- getAtPos (pos + 2)
  (n1, n2, pos') <- (,,) <$> getAtPos pointer1 <*> getAtPos pointer2 <*> getAtPos (pos + 3)
  setAtPos pos' (f n1 n2)
  pure ()

addAtPos :: Int -> State (Map.Map Int IntCode) ()
addAtPos pos = fAtPos pos (+)

multiplyAtPos :: Int -> State (Map.Map Int IntCode) ()
multiplyAtPos pos = fAtPos pos (*)

runTilEnd :: Int -> State (Map.Map Int IntCode) ()
runTilEnd pos = do
  op <- getAtPos pos
  case op of
    1 -> addAtPos pos *> runTilEnd (pos + 4)
    2 -> multiplyAtPos pos *> runTilEnd (pos + 4)
    99 -> pure ()

findInputs ops = filter (\(n1, n2) -> runIntCode n1 n2 ops == Just 19690720) [(n1, n2) | n1 <- [0 .. 99], n2 <- [0 .. 99]]

-- parseAndSolve2 :: IO ( IntCode)
parseAndSolve2 = do
  input <- runParseInput
  pure $ do
    i <- input
    let output = findInputs i
    pure $ map (\(n1, n2) -> (100 * n1) + n2) output

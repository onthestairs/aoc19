{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Intcode where

import Control.Lens
import Control.Lens.TH
import qualified Data.Map.Strict as Map
import Numbers
import Prelude hiding (Op, getOp)

type IntCode = Int

data Memory
  = Memory
      { _position :: Int,
        _inputs :: [Int],
        _values :: Map.Map Int IntCode,
        _outputs :: [Int]
      }
  deriving (Show)

$(makeLenses ''Memory)

data ParameterMode = Position | Immediate

data Op
  = Add ParameterMode ParameterMode
  | Multiply ParameterMode ParameterMode
  | JumpIfTrue ParameterMode ParameterMode
  | JumpIfFalse ParameterMode ParameterMode
  | LessThan ParameterMode ParameterMode
  | Equals ParameterMode ParameterMode
  | ReadInput
  | WriteOutput ParameterMode
  | End

getAtPos :: Int -> State Memory IntCode
getAtPos pos = do
  s <- use values
  let maybeIntCode = Map.lookup pos s
  pure $ case maybeIntCode of
    Just intCode -> intCode
    Nothing -> error "noooo"

setAtPos :: Int -> IntCode -> State Memory ()
setAtPos pos intCode = do
  modifying values (\s -> Map.insert pos intCode s)
  pure ()

getAtPosMode :: ParameterMode -> Int -> State Memory IntCode
getAtPosMode Immediate n = getAtPos n
getAtPosMode Position n = do
  n' <- getAtPos n
  getAtPos n'

runBinaryF m1 m2 f = do
  pos <- use position
  value1 <- getAtPosMode m1 (pos + 1)
  value2 <- getAtPosMode m2 (pos + 2)
  positionToSet <- getAtPos (pos + 3)
  setAtPos positionToSet (f value1 value2)
  pure ()

add m1 m2 = runBinaryF m1 m2 (+)

multiply m1 m2 = runBinaryF m1 m2 (*)

setPosToInputValue = do
  pos <- use position
  is <- use inputs
  case nonEmpty is of
    Nothing -> error "not enough inputs!"
    Just (i :| is') -> do
      newPos <- getAtPos (pos + 1)
      setAtPos newPos i
      assign inputs is'
      pure ()

writeToOutput m1 = do
  pos <- use position
  value <- getAtPosMode m1 (pos + 1)
  modifying outputs (\os -> value : os)
  pure ()

getValueAtPos :: Int -> State Memory IntCode
getValueAtPos pos = do
  s <- use values
  let maybeIntCode = Map.lookup pos s
  pure $ case maybeIntCode of
    Just intCode -> intCode
    Nothing -> error "noooo"

getModeAtIndex :: [Int] -> Int -> ParameterMode
getModeAtIndex ms i = case viaNonEmpty head (drop i ms) of
  Just 0 -> Position
  Just 1 -> Immediate
  _ -> Position

getOp :: Int -> Op
getOp n =
  let digits = toDigits n
   in case reverse digits of
        1 : 0 : modes -> Add (getModeAtIndex modes 0) (getModeAtIndex modes 1)
        1 : [] -> Add Position Position
        2 : 0 : modes -> Multiply (getModeAtIndex modes 0) (getModeAtIndex modes 1)
        2 : [] -> Multiply Position Position
        3 : [] -> ReadInput
        4 : 0 : modes -> WriteOutput (getModeAtIndex modes 0)
        4 : [] -> WriteOutput Position
        5 : 0 : modes -> JumpIfTrue (getModeAtIndex modes 0) (getModeAtIndex modes 1)
        5 : [] -> JumpIfTrue Position Position
        6 : 0 : modes -> JumpIfFalse (getModeAtIndex modes 0) (getModeAtIndex modes 1)
        6 : [] -> JumpIfFalse Position Position
        7 : 0 : modes -> LessThan (getModeAtIndex modes 0) (getModeAtIndex modes 1)
        7 : [] -> LessThan Position Position
        8 : 0 : modes -> Equals (getModeAtIndex modes 0) (getModeAtIndex modes 1)
        8 : [] -> Equals Position Position
        9 : 9 : [] -> End
        _ -> error $ "couldnt match: " <> show n

getOpAtPos :: Int -> State Memory Op
getOpAtPos pos = do
  n <- getValueAtPos pos
  pure $ getOp n

jumpIfTrue m1 m2 = do
  pos <- use position
  v1 <- getAtPosMode m1 (pos + 1)
  v2 <- getAtPosMode m2 (pos + 2)
  case v1 of
    0 -> modifying position (+ 3)
    _ -> assign position v2

jumpIfFalse m1 m2 = do
  pos <- use position
  v1 <- getAtPosMode m1 (pos + 1)
  v2 <- getAtPosMode m2 (pos + 2)
  case v1 of
    0 -> assign position v2
    _ -> modifying position (+ 3)

lessThan m1 m2 = do
  pos <- use position
  v1 <- getAtPosMode m1 (pos + 1)
  v2 <- getAtPosMode m2 (pos + 2)
  p <- getAtPos (pos + 3)
  case v1 < v2 of
    True -> setAtPos p 1
    False -> setAtPos p 0
  modifying position (+ 4)

equals m1 m2 = do
  pos <- use position
  v1 <- getAtPosMode m1 (pos + 1)
  v2 <- getAtPosMode m2 (pos + 2)
  p <- getAtPos (pos + 3)
  case v1 == v2 of
    True -> setAtPos p 1
    False -> setAtPos p 0
  modifying position (+ 4)

addOp m1 m2 = do
  add m1 m2
  modifying position (+ 4)

multiplyOp m1 m2 = do
  multiply m1 m2
  modifying position (+ 4)

readInput = do
  setPosToInputValue
  modifying position (+ 2)

writeOutput m1 = do
  writeToOutput m1
  modifying position (+ 2)

run :: State Memory ()
run = do
  pos <- use position
  op <- getOpAtPos pos
  case op of
    Add m1 m2 -> addOp m1 m2 *> run
    Multiply m1 m2 -> multiplyOp m1 m2 *> run
    ReadInput -> readInput *> run
    WriteOutput m1 -> writeOutput m1 *> run
    JumpIfTrue m1 m2 -> jumpIfTrue m1 m2 *> run
    JumpIfFalse m1 m2 -> jumpIfFalse m1 m2 *> run
    LessThan m1 m2 -> lessThan m1 m2 *> run
    Equals m1 m2 -> equals m1 m2 *> run
    End -> pure ()

runIntCode is ops = execState run (Memory {_position = 0, _inputs = is, _values = ops, _outputs = []})

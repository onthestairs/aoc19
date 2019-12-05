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
      { _inputs :: [Int],
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

fAtPos pos m1 m2 f = do
  value1 <- getAtPosMode m1 (pos + 1)
  value2 <- getAtPosMode m2 (pos + 2)
  positionToSet <- getAtPos (pos + 3)
  setAtPos positionToSet (f value1 value2)
  pure ()

-- addAtPos :: Int -> State Memory ()
addAtPos pos m1 m2 = fAtPos pos m1 m2 (+)

-- multiplyAtPos :: Int -> State Memory ()
multiplyAtPos pos m1 m2 = fAtPos pos m1 m2 (*)

setPosToInputValue pos = do
  is <- use inputs
  case nonEmpty is of
    Nothing -> pure ()
    Just (i :| is') -> do
      newPos <- getAtPos (pos + 1)
      setAtPos newPos i
      assign inputs is'
      pure ()

writeToOutput pos m1 = do
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

-- bitToParameterMode 0 = Position
-- bitToParameterMode 1 = Immediate

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

data MovePos = NewPos Int | Noop

jumpIfTrue pos m1 m2 = do
  v1 <- getAtPosMode m1 (pos + 1)
  v2 <- getAtPosMode m2 (pos + 2)
  case v1 of
    0 -> pure Noop
    _ -> pure $ NewPos v2

jumpIfFalse pos m1 m2 = do
  v1 <- getAtPosMode m1 (pos + 1)
  v2 <- getAtPosMode m2 (pos + 2)
  case v1 of
    0 -> pure $ NewPos v2
    _ -> pure Noop

lessThan pos m1 m2 = do
  v1 <- getAtPosMode m1 (pos + 1)
  v2 <- getAtPosMode m2 (pos + 2)
  p <- getAtPos (pos + 3)
  case v1 < v2 of
    True -> setAtPos p 1
    False -> setAtPos p 0

equals pos m1 m2 = do
  v1 <- getAtPosMode m1 (pos + 1)
  v2 <- getAtPosMode m2 (pos + 2)
  p <- getAtPos (pos + 3)
  case v1 == v2 of
    True -> setAtPos p 1
    False -> setAtPos p 0

runTilEnd :: Int -> State Memory ()
runTilEnd pos = do
  op <- getOpAtPos pos
  case op of
    Add m1 m2 -> addAtPos pos m1 m2 *> runTilEnd (pos + 4)
    Multiply m1 m2 -> multiplyAtPos pos m1 m2 *> runTilEnd (pos + 4)
    ReadInput -> setPosToInputValue pos *> runTilEnd (pos + 2)
    WriteOutput m1 -> writeToOutput pos m1 *> runTilEnd (pos + 3)
    JumpIfTrue m1 m2 ->
      jumpIfTrue pos m1 m2
        >>= ( \movePos -> case movePos of
                Noop -> runTilEnd (pos + 3)
                NewPos p -> runTilEnd p
            )
    JumpIfFalse m1 m2 ->
      jumpIfFalse pos m1 m2
        >>= ( \movePos -> case movePos of
                Noop -> runTilEnd (pos + 3)
                NewPos p -> runTilEnd p
            )
    LessThan m1 m2 -> lessThan pos m1 m2 *> runTilEnd (pos + 4)
    Equals m1 m2 -> equals pos m1 m2 *> runTilEnd (pos + 4)
    End -> pure ()

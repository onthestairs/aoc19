{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonadFailDesugaring #-}
{-# LANGUAGE TemplateHaskell #-}

module Intcode where

import Control.Lens
import Control.Lens.TH
import Control.Monad.State (execState)
import Control.Monad.State.Strict (MonadState)
import qualified Data.Map.Strict as Map
import Numbers
import Prelude hiding (Op, State, execState, getOp)

type IntCode = Integer

data Memory
  = Memory
      { _position :: Integer,
        _inputs :: [Integer],
        _inputsCursor :: Int,
        _values :: Map.Map Integer IntCode,
        _outputs :: [Integer],
        _relativeBaseOffset :: Integer
      }
  deriving (Show)

$(makeLenses ''Memory)

data ParameterMode = Position | Immediate | Relative deriving (Eq)

data Op
  = Add
  | Multiply
  | JumpIfTrue
  | JumpIfFalse
  | LessThan
  | Equals
  | ReadInput
  | WriteOutput
  | AdjustRelativeBase
  | End
  deriving (Eq, Show)

setAtPos :: MonadState Memory m => Integer -> IntCode -> m ()
setAtPos pos intCode = do
  modifying values (\s -> Map.insert pos intCode s)

getValue :: MonadState Memory m => Integer -> m IntCode
getValue pos = do
  s <- use values
  pure $ getValuePure pos s

getValuePure :: Integer -> Map.Map Integer IntCode -> IntCode
getValuePure pos s = Map.findWithDefault 0 pos s

getOp :: MonadState Memory m => Integer -> m (Op, [Integer], [Integer], Integer)
getOp pos = do
  vs <- use values
  relativeBase <- use relativeBaseOffset
  let n = getValuePure pos vs
  (op, posDelta) <- pure $ case (n `mod` 100) of
    1 -> (Add, 4)
    2 -> (Multiply, 4)
    3 -> (ReadInput, 2)
    4 -> (WriteOutput, 2)
    5 -> (JumpIfTrue, 3)
    6 -> (JumpIfFalse, 3)
    7 -> (LessThan, 4)
    8 -> (Equals, 4)
    9 -> (AdjustRelativeBase, 2)
    99 -> (End, 1)
    _ -> error "unknown op code!"
  let nextPos = pos + posDelta
  let modes = map (getMode n) [1 ..]
  let positionValues = map (flip getValuePure vs) [pos + 1 ..]
  let positionPointers = zipWith (\mode position -> if mode == Relative then relativeBase + position else position) modes positionValues
  let opValues =
        zipWith
          ( \mode value -> case mode of
              Immediate -> value
              _ -> getValuePure value vs
          )
          modes
          positionPointers
  pure $ (op, positionPointers, opValues, nextPos)

getMode :: Integer -> Integer -> ParameterMode
getMode n i = case (n `div` (10 ^ (i + 1))) `mod` 10 of
  0 -> Position
  1 -> Immediate
  2 -> Relative
  _ -> error "cant get mode!"

readInput :: MonadState Memory m => m Integer
readInput = do
  is <- use inputs
  inputsPos <- use inputsCursor
  case is ^? (ix inputsPos) of
    Nothing -> error "not enough inputs!"
    Just i -> do
      modifying inputsCursor (+ 1)
      pure i

writeToOutputs :: MonadState Memory m => Integer -> m ()
writeToOutputs = (outputs %=) . (:)

run :: MonadState Memory m => m ()
run = do
  pos <- use position
  (op, (p1 : _ : p3 : _), (rv1 : rv2 : rv3 : _), nextPos) <- getOp pos
  if op == End
    then pure ()
    else do
      case op of
        Add -> setAtPos p3 (rv1 + rv2)
        Multiply -> setAtPos p3 (rv1 * rv2)
        ReadInput -> readInput >>= \input -> setAtPos p1 input
        LessThan -> setAtPos p3 (if rv1 < rv2 then 1 else 0)
        Equals -> setAtPos p3 (if rv1 == rv2 then 1 else 0)
        _ -> pure ()
      assign position $ case op of
        JumpIfTrue -> if rv1 /= 0 then rv2 else nextPos
        JumpIfFalse -> if rv1 == 0 then rv2 else nextPos
        _ -> nextPos
      case op of
        AdjustRelativeBase -> modifying relativeBaseOffset (+ rv1)
        _ -> pure ()
      run
      when (op == WriteOutput) $ writeToOutputs rv1

makeInitialMemory ops = Memory {_position = 0, _inputs = [], _inputsCursor = 0, _values = ops, _outputs = [], _relativeBaseOffset = 0}

runIntCode is ops = execState run $ (makeInitialMemory ops) & inputs .~ is

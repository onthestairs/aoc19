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

type IntCode = Int

data Memory
  = Memory
      { _position :: Int,
        _inputs :: [Int],
        _inputsCursor :: Int,
        _values :: Map.Map Int IntCode,
        _outputs :: [Int]
      }
  deriving (Show)

$(makeLenses ''Memory)

data ParameterMode = Position | Immediate deriving (Eq)

data Op
  = Add
  | Multiply
  | JumpIfTrue
  | JumpIfFalse
  | LessThan
  | Equals
  | ReadInput
  | WriteOutput
  | End
  deriving (Eq, Show)

setAtPos :: MonadState Memory m => Int -> IntCode -> m ()
setAtPos pos intCode = do
  modifying values (\s -> Map.insert pos intCode s)

getValue :: MonadState Memory m => Int -> m IntCode
getValue pos = do
  s <- use values
  pure $ getValuePure pos s

getValuePure :: Int -> Map.Map Int IntCode -> IntCode
getValuePure pos s = case Map.lookup pos s of
  Just intCode -> intCode
  Nothing -> error "noooo"

getOp :: MonadState Memory m => Int -> m (Op, [Int], [Int], Int)
getOp pos = do
  vs <- use values
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
    99 -> (End, 1)
    _ -> error "unknown op code!"
  let nextPos = pos + posDelta
  let modes = map (getMode n) [1 ..]
  let immediateValues = map (flip getValuePure vs) [pos + 1 ..]
  let opValues = zipWith (\mode value -> if mode == Immediate then value else getValuePure value vs) modes immediateValues
  pure (op, immediateValues, opValues, nextPos)

getMode :: Int -> Int -> ParameterMode
getMode n i = case (n `div` (10 ^ (i + 1))) `mod` 10 of
  0 -> Position
  1 -> Immediate
  _ -> error "cant get mode!"

readInput :: MonadState Memory m => m Int
readInput = do
  is <- use inputs
  inputsPos <- use inputsCursor
  case is ^? (ix (trace ("inputpos: " <> show inputsPos) inputsPos)) of
    Nothing -> error "not enough inputs!"
    Just i -> do
      modifying inputsCursor (+ 1)
      pure i

writeToOutputs :: MonadState Memory m => Int -> m ()
writeToOutputs = (outputs %=) . (:)

run :: MonadState Memory m => m ()
run = do
  pos <- use position
  (op, (p1 : _ : p3 : _), (rv1 : rv2 : _), nextPos) <- getOp pos
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
      run
      when (op == WriteOutput) $ writeToOutputs (trace ("outputting... " <> show rv1) rv1)

makeInitialMemory ops = Memory {_position = 0, _inputs = [], _inputsCursor = 0, _values = ops, _outputs = []}

runIntCode is ops = execState run $ (makeInitialMemory ops) & inputs .~ is

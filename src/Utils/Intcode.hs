module Utils.Intcode (
  IntcodeComputer,
  Program,
  makeIC,
  execProgram,
  getMemoryAt,
  getOutput
) where

import           Control.Monad.State.Strict (State, execState, gets, modify,
                                             unless)
import           Data.IntMap.Strict         (IntMap, (!))
import qualified Data.IntMap.Strict         as IntMap

data IntcodeComputer = IC {
  icMemory  :: IntMap Integer,
  icPointer :: Int,
  icModeNum :: Int,
  icInput   :: [Integer],
  icOutput  :: [Integer],
  icHalted  :: Bool
}

data ParameterMode = Immediate | Position

type Program = [Integer]

makeIC :: Program -> [Integer] -> IntcodeComputer
makeIC memory input = IC {
  icMemory = IntMap.fromList $ zip [0 ..] memory,
  icPointer = 0,
  icModeNum = 0,
  icInput = input,
  icOutput = [],
  icHalted = False
}

execProgram :: Program -> [Integer] -> IntcodeComputer
execProgram program = execComputer . makeIC program

-- Operations

execComputer :: IntcodeComputer -> IntcodeComputer
execComputer = execState execProgramState

execProgramState :: State IntcodeComputer ()
execProgramState = do
  halted <- gets icHalted
  unless halted $ do
    opCode <- readImmediate
    let (mode, opCode') = parseModeOp opCode
    setModeNum mode
    case opCode' of
      99 -> haltOp
      1  -> addOp
      2  -> mulOp
      3  -> inputOp
      4  -> outputOp
      _  -> error $ show opCode' ++ " not yet implemented."
    execProgramState

haltOp :: State IntcodeComputer ()
haltOp = modify (\s -> s{icHalted = True})

addOp :: State IntcodeComputer ()
addOp = do
  op1 <- readParameter
  op2 <- readParameter
  writeParameter (op1 + op2)

mulOp :: State IntcodeComputer ()
mulOp = do
  op1 <- readParameter
  op2 <- readParameter
  writeParameter (op1 * op2)

inputOp :: State IntcodeComputer ()
inputOp = do
  input <- consumeInput
  writeParameter input

outputOp :: State IntcodeComputer ()
outputOp = do
  val <- readParameter
  appendOutput val

-- General utils for interacting with IntcodeComputer state -----------------------------

-- Set pointer to the given value
setPointer :: Int -> State IntcodeComputer ()
setPointer p = modify (\s -> s{icPointer = p})

-- Increments pointer by 1
incPointer :: State IntcodeComputer ()
incPointer = modify (\s -> s{icPointer = icPointer s + 1})

-- Reads the memory at the given address, without modifying pointer.
readValue :: Int -> State IntcodeComputer Integer
readValue address = do
  memory <- gets icMemory
  return (memory ! address)

-- Writes a value to the given address, without modifying the pointer.
writeValue :: Int -> Integer -> State IntcodeComputer ()
writeValue address value = modify (\s -> s{icMemory = IntMap.insert address value $ icMemory s})

setModeNum :: Int -> State IntcodeComputer ()
setModeNum mode = modify (\s -> s{icModeNum = mode})

-- Retrieves the mode to be used for the next parameter. Shifts the mode one position to the right.
getParameterMode :: State IntcodeComputer ParameterMode
getParameterMode = do
  modeNum <- gets icModeNum
  let mode = parseMode (modeNum `mod` 10)
  modify (\s -> s{icModeNum = modeNum `div` 10})
  return mode

consumeInput :: State IntcodeComputer Integer
consumeInput = do
  input <- gets icInput
  case input of
    []       -> error "No input remaining."
    (x : xs) -> do
      modify (\s -> s{icInput = xs})
      return x

appendOutput :: Integer -> State IntcodeComputer ()
appendOutput val = modify (\s -> s{icOutput = val : icOutput s})

-- Functions for interacting with parameters ------------------------------------------

readParameter :: State IntcodeComputer Integer
readParameter = do
  mode <- getParameterMode
  case mode of
    Immediate -> readImmediate
    Position  -> readPosition

-- Reads a parameter using immediate addressing. Increments pointer.
readImmediate :: State IntcodeComputer Integer
readImmediate = do
  pointer <- gets icPointer
  val <- readValue pointer
  incPointer
  return val

-- Reads a parameter using position addressing. Increments pointer.
readPosition :: State IntcodeComputer Integer
readPosition = do
  pointer <- gets icPointer
  address <- fromInteger <$> readValue pointer
  val <- readValue address
  incPointer
  return val

writeParameter :: Integer -> State IntcodeComputer ()
writeParameter val = do
  mode <- getParameterMode
  case mode of
    Immediate -> error "Writes cannot use immediate addressing."
    Position  -> writePosition val

writePosition :: Integer -> State IntcodeComputer ()
writePosition val = do
  pointer <- gets icPointer
  address <- fromInteger <$> readValue pointer
  writeValue address val
  incPointer

-- General utils not using state -------------------------

parseModeOp :: Integer -> (Int, Int)
parseModeOp opCode = (fromInteger $ opCode `div` 100, fromInteger $ opCode `mod` 100)

parseMode :: Int -> ParameterMode
parseMode n = case n of
  0 -> Position
  1 -> Immediate
  _ -> error $ "No such parameter mode: " ++ show n

-- Getters

getMemoryAt :: IntcodeComputer -> Int -> Integer
getMemoryAt = (!) . icMemory

getOutput :: IntcodeComputer -> [Integer]
getOutput = icOutput

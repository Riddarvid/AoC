{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Days.Day17 (solve) where
import           AoCUtils.Days       (Solver)
import           AoCUtils.Regex      (parseSignedInts)
import           Control.Monad.State (State, gets, modify)
import           Data.Bits           (Bits (xor))
import           Data.List           (intercalate)
import           Data.Maybe          (fromJust)
import           Data.Sequence       (Seq)
import qualified Data.Sequence       as Seq
import           Prelude             hiding (exponent, seq)

solve :: Solver
solve input = let
  (a, program) = parseInput input
  part1 = solve1 a
  part2 = solve2 program
  in (part1, show part2)

parseInput :: String -> (Integer, [Int])
parseInput input = (a, program)
  where
    rows = lines input
    a = head $ parseSignedInts (rows !! 0)
    program = parseSignedInts (rows !! 4)

parseInputOld :: String -> MachineState
parseInputOld input = MS {
  msA = a,
  msB = b,
  msC = c,
  msProgram = program,
  msPC = 0}
  where
    rows = lines input
    a = head . parseSignedInts $ rows !! 0
    b = head . parseSignedInts $ rows !! 1
    c = head . parseSignedInts $ rows !! 2
    program = Seq.fromList $ parseSignedInts (rows !! 4)

solve1 :: Integer -> String
solve1 = intercalate "," . map show . runCompiledProgram

solve2 :: [Int] -> Integer
solve2 program = minimum initAs
  where
    aSeqs = foldr appendPossibleSequences [[]] program
    initAs = map head aSeqs

--------- Compiled to Haskell ---------

runCompiledProgram :: Integer -> [Int]
runCompiledProgram a
  | a == 0 = []
  | otherwise = outC : runCompiledProgram a'
  where
    (outC, a') = stepCompiledProgram a

stepCompiledProgram :: Integer -> (Int, Integer)
stepCompiledProgram a = let
  b' = a `mod` 8
  b'' = xor b' 1
  c = a `div` (2 ^ b'')
  b''' = xor b'' 4
  b'''' = xor b''' c
  outC = fromInteger (b'''' `mod` 8)
  in (outC, a `div` 8)

appendPossibleSequences :: Int -> [[Integer]] -> [[Integer]]
appendPossibleSequences = concatMap . tryAppend

tryAppend :: Int -> [Integer] -> [[Integer]]
tryAppend target seq = map (: seq) viableAs
  where
    possibleAs = case seq of
      []          -> [1 ..7]
      (lastA : _) -> let last8A = lastA * 8 in [last8A .. last8A + 7]
    viableAs = filter (\a -> fst (stepCompiledProgram a) == target) possibleAs

--------- Machine ------------------
-- Not currently used but kept anyways

data MachineState = MS {
  msA       :: Integer,
  msB       :: Integer,
  msC       :: Integer,
  msProgram :: Seq Int,
  msPC      :: Int
}

runProgram :: State MachineState [Int]
runProgram = do
  instr' <- readOpcode
  case instr' of
    Nothing    -> return []
    Just instr -> do
      operand <- readOperand
      mc <- exec instr operand
      case mc of
        Nothing -> runProgram
        Just c  -> (c :) <$> runProgram

exec :: Int -> Int -> State MachineState (Maybe Int)
exec = \case
  0 -> adv
  1 -> bxl
  2 -> bst
  3 -> jnz
  4 -> bxc
  5 -> out
  6 -> bdv
  7 -> cdv
  _ -> error "Invalid opcode"

adv :: Int -> State MachineState (Maybe Int)
adv operand = do
  result <- expDiv operand
  modify (\ms -> ms{msA = result})
  incPC
  return Nothing

bxl :: Int -> State MachineState (Maybe Int)
bxl operand1 = do
  operand2 <- gets msB
  let result = xor (toInteger operand1) operand2
  modify (\ms -> ms{msB = result})
  incPC
  return Nothing

bst :: Int -> State MachineState (Maybe Int)
bst operand = do
  val <- readComboOperand operand
  let result = val `mod` 8
  modify (\ms -> ms{msB = result})
  incPC
  return Nothing

jnz :: Int -> State MachineState (Maybe Int)
jnz operand = do
  cond <- gets msA
  if cond == 0
    then do
      incPC
    else do
      modify (\ms -> ms{msPC = operand})
  return Nothing

bxc :: Int -> State MachineState (Maybe Int)
bxc _ = do
  op1 <- gets msB
  op2 <- gets msC
  let result = xor op1 op2
  modify (\ms -> ms{msB = result})
  incPC
  return Nothing

out :: Int -> State MachineState (Maybe Int)
out operand = do
  c <- readComboOperand operand
  let c' = fromInteger (c `mod` 8)
  incPC
  return $ Just c'

bdv :: Int -> State MachineState (Maybe Int)
bdv operand = do
  result <- expDiv operand
  modify (\ms -> ms{msB = result})
  incPC
  return Nothing

cdv :: Int -> State MachineState (Maybe Int)
cdv operand = do
  result <- expDiv operand
  modify (\ms -> ms{msC = result})
  incPC
  return Nothing

expDiv :: Int -> State MachineState Integer
expDiv operand = do
  numerator <- gets msA
  exponent <- readComboOperand operand
  let denominator = 2 ^ exponent
  let result = numerator `div` denominator
  return result

incPC :: State MachineState ()
incPC = modify (\ms -> ms{msPC = msPC ms + 2})

readOpcode :: State MachineState (Maybe Int)
readOpcode = do
  pc <- gets msPC
  readFromAddress pc

readOperand :: State MachineState Int
readOperand = do
  pc <- gets msPC
  fromJust <$> readFromAddress (pc + 1)

readFromAddress :: Int -> State MachineState (Maybe Int)
readFromAddress addr = do
  program <- gets msProgram
  return $ Seq.lookup addr program

readComboOperand :: Int -> State MachineState Integer
readComboOperand = \case
  0 -> return 0
  1 -> return 1
  2 -> return 2
  3 -> return 3
  4 -> gets msA
  5 -> gets msB
  6 -> gets msC
  7 -> error "Reserved"
  _ -> error "Invalid combo operand"

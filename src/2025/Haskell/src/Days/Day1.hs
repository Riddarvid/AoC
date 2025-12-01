module Days.Day1 (solve) where
import           AoCUtils.Days (Solver)

data Dir = L | R
  deriving (Show)

data Instruction = Instruction Dir Int
  deriving (Show)

solve :: Solver
solve input = let
  instructions = parseInstructions input
  part1 = solve1 instructions
  part2 = solve2 instructions
  in (show part1, show part2)

parseInstructions :: String -> [Instruction]
parseInstructions = map parseInstruction . lines

parseInstruction :: String -> Instruction
parseInstruction (dirChar : distanceLine) = Instruction dir (read distanceLine)
  where
    dir = case dirChar of
      'L' -> L
      'R' -> R
      _   -> error "Illegal char"
parseInstruction [] = error "Empty line"

solve1 :: [Instruction] -> Int
solve1 instructions = length $ filter (== 0) positions
  where
    positions = foldl (baseAppend (\e delta -> (e + delta) `mod` 100)) [50] instructions

solve2 :: [Instruction] -> Int
solve2 instructions = wholeRotationCount + length (filter snd positionsAndClickedZero)
  where
    positionsAndClickedZero = foldl (baseAppend findNextAndCheckZero) [(50, False)] nonWholeRotationInstructions
    wholeRotationCount = sum $ map (\(Instruction _ d) -> d `div` 100) instructions
    nonWholeRotationInstructions = map (\(Instruction dir d) -> Instruction dir (d `mod` 100)) instructions

baseAppend :: (a -> Int -> a) -> [a] -> Instruction -> [a]
baseAppend _ [] _ = error "No last position found"
baseAppend nextElement poss@(lastPos : _) (Instruction dir distance) = newPos : poss
  where
    delta = case dir of
      L -> -distance
      R -> distance
    newPos = nextElement lastPos delta

-- delta is guaranteed to be between -99 and 99
findNextAndCheckZero :: (Int, Bool) -> Int -> (Int, Bool)
findNextAndCheckZero (pos, _) delta
  | delta == 0 = (pos, False)
  | pos == 0 = (newPos, False)
  | otherwise = (newPos, clickedZero)
  where
    newPosRaw = pos + delta
    newPos = newPosRaw `mod` 100
    clickedZero = newPosRaw > 99 || newPosRaw <= 0

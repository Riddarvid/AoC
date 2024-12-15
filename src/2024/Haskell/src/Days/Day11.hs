module Days.Day11 (solve) where
import           AoCUtils.Days         (Solver)
import           AoCUtils.Regex        (parseUnsignedInts)
import           Data.Function.Memoize (memoize2)

solve :: Solver
solve input = let
  stones = parseStones input
  part1 = solve1 stones
  part2 = solve2 stones
  in (show part1, show part2)

parseStones :: String -> [Integer]
parseStones = parseUnsignedInts

solve1 :: [Integer] -> Integer
solve1 = totalStonesAfter 25

solve2 :: [Integer] -> Integer
solve2 = totalStonesAfter 75

totalStonesAfter :: Int -> [Integer] -> Integer
totalStonesAfter n stones = toInteger (length stones) + sum (map (nNewStonesAfterMemo n) stones)

nNewStonesAfterMemo :: Int -> Integer -> Integer
nNewStonesAfterMemo = memoize2 nNewStonesAfter

nNewStonesAfter :: Int -> Integer -> Integer
nNewStonesAfter 0 _     = 0
nNewStonesAfter n stone = nNew + sum (map (nNewStonesAfterMemo (n - 1)) newStones)
  where
    newStones = blink stone
    nNew = toInteger $ length newStones - 1

blink :: Integer -> [Integer]
blink stone
  | stone == 0 = [1]
  | even nLength = let (stone1, stone2) = splitAt (nLength `div` 2) nString in [read stone1, read stone2]
  | otherwise = [stone * 2024]
  where
    nString = show stone
    nLength = length nString

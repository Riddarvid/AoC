module Days.Day3 (solve) where
import           AoCUtils.Days (Solver)
import           Data.Char     (digitToInt)

-- Process:
-- 1. Find the leftmost instance of the highest value in the first n-1 cells.
-- 2. Find the highest value to the right of that
type BatteryBank = [Int]

solve :: Solver
solve input = let
  batteryBanks = map (map digitToInt) $ lines input
  part1 = solve1 batteryBanks
  part2 = solve2 batteryBanks
  in (show part1, show part2)

solve1 :: [BatteryBank] -> Integer
solve1 = sum . map (findMaxJoltage 2)

solve2 :: [BatteryBank] -> Integer
solve2 = sum . map (findMaxJoltage 12)

findMaxJoltage :: Int -> BatteryBank -> Integer
findMaxJoltage 0 _    = 0
findMaxJoltage n bank = toInteger firstDigit * (10 ^ (n - 1)) + subJoltage
  where
    firstDigit = maximum $ dropTail (n - 1) bank
    subJoltage = findMaxJoltage (n - 1) $ tail $ dropWhile (/= firstDigit) bank

-- Copied from ghc
dropTail :: Int -> [a] -> [a]
dropTail n _xs
  = go (drop n _xs) _xs
  where
    go (_:ys) (x:xs) = x : go ys xs
    go _      _      = []  -- Stop when ys runs out
                           -- It'll always run out before xs does

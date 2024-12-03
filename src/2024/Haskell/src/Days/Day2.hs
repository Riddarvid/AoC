module Days.Day2 (solve) where
import           AoCUtils.Days  (Solver)
import           AoCUtils.Regex (parseUnsignedInts)
import           Data.List      (partition)

solve :: Solver
solve input = let
  reports = parseReports input
  part1 = solve1 reports
  part2 = solve2 reports
  in (show part1, show part2)

parseReports :: String -> [[Integer]]
parseReports = map parseUnsignedInts . lines

solve1 :: [[Integer]] -> Int
solve1 = length . filter isSafe

isSafe :: [Integer] -> Bool
isSafe report = (increasing || decreasing) && safeSteps
  where
    diffs = diffList report
    increasing = all (> 0) diffs
    decreasing = all (< 0) diffs
    safeSteps = all ((\diff -> 1 <= diff && diff <= 3) . abs) diffs

solve2 :: [[Integer]] -> Int
solve2 reports = length safe + length (filter isAlmostSafe unsafe)
  where
    (safe, unsafe) = partition isSafe reports

isAlmostSafe :: [Integer] -> Bool
isAlmostSafe report = any isSafe subLists
  where
    subLists = createSubLists report

createSubLists :: [Integer] -> [[Integer]]
createSubLists []       = []
createSubLists (x : xs) = xs : map (x :) (createSubLists xs)


diffList :: Num a => [a] -> [a]
diffList []       = []
diffList (x : xs) = zipWith (-) xs (x : xs)

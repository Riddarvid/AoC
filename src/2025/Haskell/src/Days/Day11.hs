module Days.Day11 (solve) where
import           AoCUtils.Days         (Solver)
import           Data.Function.Memoize (Memoizable (memoize))
import           Data.HashMap.Lazy     (HashMap)
import qualified Data.HashMap.Lazy     as HM

type Graph = HashMap String [String]

solve :: Solver
solve input = let
  graph = parseGraph input
  part1 = solve1 graph :: Int
  part2 = solve2 graph :: Integer
  in (show part1, show part2)

parseGraph :: String -> Graph
parseGraph = foldr parseRow HM.empty . lines

parseRow :: String -> Graph -> Graph
parseRow row = HM.insert key values
  where
    words' = words row
    key = init $ head words'
    values = tail words'

solve1 :: Num a => Graph -> a
solve1 graph = findNumberOfPaths graph "you" "out"

-- Alternative solution:
-- Find all paths from srv to dac and then dac to fft and then fft to out.
-- Multiply these path numbers to get the total number of paths.
-- Then do the same for srv -> fft -> dac -> out
solve2 :: Num a => Graph -> a
solve2 graph =
  findNumberOfCompositePaths graph ["svr", "dac", "fft", "out"] +
  findNumberOfCompositePaths graph ["svr", "fft", "dac", "out"]

findNumberOfCompositePaths :: Num a => Graph -> [String] -> a
findNumberOfCompositePaths graph compositePath = product numberOfPathss
  where
    pairs = mkPairs compositePath
    numberOfPathss = map (uncurry $ findNumberOfPaths graph) pairs

findNumberOfPaths :: Num a => Graph -> String -> String -> a
findNumberOfPaths graph start goal = memoGo start
  where
    memoGo = memoize go
    go current
      | current == goal = 1
      | otherwise = case HM.lookup current graph of
        Nothing        -> 0
        Just nextNodes -> sum $ map memoGo nextNodes

mkPairs :: [a] -> [(a, a)]
mkPairs (x : y : zs) = (x, y) : mkPairs (y : zs)
mkPairs _            = []

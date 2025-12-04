module Days.Day4 (solve) where
import           AoCUtils.Days     (Solver)
import           AoCUtils.Geometry (Point2 (P2))
import           AoCUtils.Matrices (matrixToMapList)
import           Data.HashSet      (HashSet)
import qualified Data.HashSet      as HS

solve :: Solver
solve input = let
  rollPoints = parseRollMap input
  part1 = solve1 rollPoints
  part2 = solve2 rollPoints
  in (show part1, show part2)

parseRollMap :: String -> HashSet (Point2 Int)
parseRollMap input = HS.fromList $ map fst $ filter (\(_, c) -> c == '@') points
  where
    (points, _, _) = matrixToMapList $ lines input

solve1 :: HashSet (Point2 Int) -> Int
solve1 = HS.size . findAccessible

solve2 :: HashSet (Point2 Int) -> Int
solve2 rolls = HS.size (HS.difference rolls lastState)
  where
    rollStates = iterate (\s -> HS.difference s (findAccessible s)) rolls
    lastState = last $ takeWhileDifferent rollStates

findAccessible :: HashSet (Point2 Int) -> HashSet (Point2 Int)
findAccessible rolls = HS.filter (\p -> length (neighbors rolls p) < 4) rolls

neighbors :: HashSet (Point2 Int) -> Point2 Int -> [Point2 Int]
neighbors rolls (P2 x y) = filter (`HS.member` rolls) allNegighbors
  where
    allNegighbors = [
      P2 (x - 1) (y - 1),
      P2 x (y - 1),
      P2 (x + 1) (y - 1),
      P2 (x - 1) y,
      P2 (x + 1) y,
      P2 (x - 1) (y + 1),
      P2 x (y + 1),
      P2 (x + 1) (y + 1)
      ]

takeWhileDifferent :: Eq a => [a] -> [a]
takeWhileDifferent (x : y : xs)
  | x == y = [x]
  | otherwise = x : takeWhileDifferent (y : xs)
takeWhileDifferent xs = xs

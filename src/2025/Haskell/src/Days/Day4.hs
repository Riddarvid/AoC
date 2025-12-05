{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Days.Day4 (solve) where
import           AoCUtils.Days     (Solver)
import           AoCUtils.Geometry (Point2 (P2))
import           AoCUtils.Matrices (matrixToMapList)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import           Data.HashSet      (HashSet)
import qualified Data.HashSet      as HS

solve :: Solver
solve input = let
  rollPoints = parseRollMap input
  rollCounts = countAllNeighbors rollPoints
  part1 = solve1 rollPoints
  part2 = solve2Alt rollCounts
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
    lastState = last $ takeWhileBi (\a b -> HS.size a /= HS.size b) rollStates

findAccessible :: HashSet (Point2 Int) -> HashSet (Point2 Int)
findAccessible rolls = HS.filter (\p -> length (neighbors rolls p) < 4) rolls

neighbors :: HashSet (Point2 Int) -> Point2 Int -> [Point2 Int]
neighbors rolls = filter (`HS.member` rolls) . allNeighbors

allNeighbors :: Point2 Int -> [Point2 Int]
allNeighbors (P2 x y) = [
  P2 (x - 1) (y - 1),
  P2 x (y - 1),
  P2 (x + 1) (y - 1),
  P2 (x - 1) y,
  P2 (x + 1) y,
  P2 (x - 1) (y + 1),
  P2 x (y + 1),
  P2 (x + 1) (y + 1)
  ]

takeWhileBi :: (a -> a -> Bool) -> [a] -> [a]
takeWhileBi p (x : y : xs)
  | p x y = x : takeWhileBi p (y : xs)
  | otherwise = [x]
takeWhileBi _ xs = xs

countAllNeighbors :: HashSet (Point2 Int) -> HashMap (Point2 Int) Int
countAllNeighbors rolls =
  HM.mapWithKey (\p _ -> length $ neighbors rolls p) $ HS.toMap rolls

solve2Alt :: HashMap (Point2 Int) Int -> Int
solve2Alt rollCounts = HM.size (HM.difference rollCounts lastState)
  where
    rollStates = iterate removeAccessible rollCounts
    lastState = last $ takeWhileBi (\a b -> HM.size a /= HM.size b) rollStates

removeAccessible :: HashMap (Point2 Int) Int -> HashMap (Point2 Int) Int
removeAccessible rollCounts =
  HM.foldrWithKey reduceNeighborCounts (HM.filter (>= 4) rollCounts) toRemove
  where
    toRemove = HM.filter (< 4) rollCounts

reduceNeighborCounts :: Point2 Int -> Int -> HashMap (Point2 Int) Int -> HashMap (Point2 Int) Int
reduceNeighborCounts toRemove _ rollCounts = foldr reduceCount rollCounts $ allNeighbors toRemove

reduceCount :: Point2 Int -> HashMap (Point2 Int) Int -> HashMap (Point2 Int) Int
reduceCount = HM.adjust (\v -> v - 1)

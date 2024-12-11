module Days.Day10 (solve) where
import           AoCUtils.Days     (Solver)
import           AoCUtils.Geometry (Point (moveBy), Point2, downV, leftV,
                                    rightV, upV)
import           AoCUtils.Matrices (matrixToMapList)
import           Data.Char         (digitToInt)
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Set          (Set)
import qualified Data.Set          as Set

solve :: Solver
solve input = let
  topoMap = parseTopoMap input
  trailheads = findTrailheads topoMap
  part1 = solve1 topoMap trailheads
  part2 = solve2 topoMap trailheads
  in (show part1, show part2)

type TopoMap = Map (Point2 Int) Int

parseTopoMap :: String -> TopoMap
parseTopoMap str = Map.map digitToInt $ Map.fromList mapList
  where
    (mapList, _, _) = matrixToMapList $ lines str

solve1 :: TopoMap -> [Point2 Int] -> Int
solve1 topoMap = sum . map (trailheadScore1 topoMap)

solve2 :: TopoMap -> [Point2 Int] -> Int
solve2 topoMap = sum . map (trailheadScore2 topoMap)

findTrailheads :: TopoMap -> [Point2 Int]
findTrailheads = Map.keys . Map.filter (== 0)

trailheadScore1 :: TopoMap -> Point2 Int -> Int
trailheadScore1 topoMap = Set.size . trailheadEnds topoMap

trailheadEnds :: TopoMap -> Point2 Int -> Set (Point2 Int)
trailheadEnds topoMap = go
  where
    go point = case Map.lookup point topoMap of
      Nothing -> Set.empty
      Just height -> if height == 9
        then Set.singleton point
        else Set.unions $ map go validNeighbors
        where
          validNeighbors = filter (hasGradualAscent topoMap height) neighbors
          neighbors = map (moveBy point) [upV, rightV, downV, leftV]

trailheadScore2 :: TopoMap -> Point2 Int -> Int
trailheadScore2 topoMap = go
  where
    go point = case Map.lookup point topoMap of
      Nothing -> 0
      Just height -> if height == 9
        then 1
        else sum $ map go validNeighbors
        where
          validNeighbors = filter (hasGradualAscent topoMap height) neighbors
          neighbors = map (moveBy point) [upV, rightV, downV, leftV]

hasGradualAscent :: TopoMap -> Int -> Point2 Int -> Bool
hasGradualAscent topoMap oldHeight p = case Map.lookup p topoMap of
  Nothing     -> False
  Just height -> height - oldHeight == 1

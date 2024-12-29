module Days.Day20 (solve) where
import           AoCUtils.Days     (Solver)
import           AoCUtils.Geometry (Point (distanceBetween), Point2 (P2))
import           AoCUtils.Graphs   (Goal (GTarget), bfsPath)
import           AoCUtils.Matrices (matrixToMapList)
import           Data.Foldable     (find)
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Maybe        (fromJust, mapMaybe)
import           Data.Set          (Set)
import qualified Data.Set          as Set
import           Utils             (neighborsOf)

-- Note: A cheat is defined only by its start and end positions, not the path it uses.
-- This is good, because it already works with my solution from part 1.
solve :: Solver
solve input = let
  (trackSet, start, end) = parseInput input
  distanceMap = buildDistanceMap trackSet start end
  part1 = solve1 distanceMap
  part2 = solve2 distanceMap
  in (show part1, show part2)

parseInput :: String -> (Set (Point2 Int), Point2 Int, Point2 Int)
parseInput input = (trackSet, start, end)
  where
    (mapList, _, _) = matrixToMapList $ lines input
    trackSet = Set.fromList $ map fst $ filter (\(_, c) -> c /= '#') mapList
    start = fst $ fromJust $ find (\(_, c) -> c == 'S') mapList
    end = fst $ fromJust $ find (\(_, c) -> c == 'E') mapList

solve1 :: Map (Point2 Int) Int -> Int
solve1 = length . filter (>= 100) . findCheats 2

solve2 :: Map (Point2 Int) Int -> Int
solve2 = length . filter (>= 100) . findCheats 20



-- Maps from tile to distance from START
buildDistanceMap :: Set (Point2 Int) -> Point2 Int -> Point2 Int -> Map (Point2 Int) Int
buildDistanceMap track start end = Map.fromList $ zip path [0 ..]
  where
    path = fromJust $ bfsPath start (GTarget end) $ neighborsFun track

neighborsFun :: Set (Point2 Int) -> Point2 Int -> [Point2 Int]
neighborsFun track point = filter (`Set.member` track) neighbors
  where
    neighbors = neighborsOf point

findCheats :: Int -> Map (Point2 Int) Int -> [Int]
findCheats cheatDistance distanceMap =
  concatMap (uncurry $ localCheats cheatDistance distanceMap) distanceList
  where
    distanceList = Map.toList distanceMap

localCheats :: Int -> Map (Point2 Int) Int -> Point2 Int -> Int -> [Int]
localCheats cheatDistance distanceMap point distance = gains
  where
    neighbors = neighborsWithin cheatDistance point
    gains = mapMaybe (findGain distanceMap point distance) neighbors

findGain :: Map (Point2 Int) Int -> Point2 Int -> Int -> Point2 Int -> Maybe Int
findGain distanceMap cheatStart distanceToStart cheatEnd = do
  originalDistanceToEnd <- Map.lookup cheatEnd distanceMap
  return $ originalDistanceToEnd - distanceToStart - cheatDistance
  where
    cheatDistance = distanceBetween cheatStart cheatEnd

neighborsWithin :: (Enum a, Num a) => a -> Point2 a -> [Point2 a]
neighborsWithin d (P2 oX oY) =
  [P2 (oX + dx) (oY + dy) | dx <- [(-d) .. d], let d' = d - abs dx, dy <- [(-d') .. d']]

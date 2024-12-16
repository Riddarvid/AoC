module Days.Day12 (solve) where
import           AoCUtils.Days     (Solver)
import           AoCUtils.Geometry (Point (moveBy), Point2 (p2Y), downV, leftV,
                                    rightV, upV)
import           AoCUtils.Graphs   (BfsState (bfsPreMap), Goal (GFull),
                                    bfsExplore)
import           AoCUtils.Matrices (matrixToMapList)
import           Data.Foldable     (find)
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Maybe        (fromJust)
import           Data.Set          (Set)
import qualified Data.Set          as Set
import           Debug.Trace       (trace, traceShow, traceShowId)
import           Utils             (Direction, moveByDir, neighborDirections,
                                    neighborsOf, turnDirLeft, turnDirRight)

solve :: Solver
solve input = let
  plantMap = parsePlantMap input
  regions = buildRegions plantMap
  part1 = solve1 regions
  part2 = solve2 regions
  in (show part1, show part2)

type PlantMap = Map (Point2 Int) Char
type Region = Set (Point2 Int)

parsePlantMap :: String -> PlantMap
parsePlantMap input = Map.fromList mapList
  where
    (mapList, _, _) = matrixToMapList $ lines input

------ Construction of regions ------------------------

buildRegions :: PlantMap -> [Region]
buildRegions plantMap = case Map.toList plantMap of
  []          -> []
  ((point, plant) : _) -> let
    (region, plantMap') = extractRegion plantMap plant point
    in region : buildRegions plantMap'

extractRegion :: PlantMap -> Char -> Point2 Int -> (Region, PlantMap)
extractRegion plantMap plantType start = (region, plantMap')
  where
    bfsState = fromJust $ bfsExplore start GFull (regionAdjacent plantType plantMap)
    region = Map.keysSet $ bfsPreMap bfsState
    plantMap' = Map.filterWithKey (\point _ -> not $ Set.member point region) plantMap

regionAdjacent :: Char -> PlantMap -> Point2 Int -> [Point2 Int]
regionAdjacent plantType plantMap point = regionNeighbors
  where
    neighbors = map (moveBy point) [upV, rightV, downV, leftV]
    regionNeighbors = filter (isRegionNeighbor plantType plantMap) neighbors

isRegionNeighbor :: Char -> PlantMap -> Point2 Int -> Bool
isRegionNeighbor plantType plantMap point = case Map.lookup point plantMap of
  Just t -> plantType == t
  _      -> False

------ Solve ------------------------------------------

solve1 :: [Region] -> Integer
solve1 = sum . map fencePrice1

solve2 :: [Region] -> Integer
solve2 = sum . map (traceShowId . fencePrice2)

------ Pricing ----------------------------------------

fencePrice1 :: Region -> Integer
fencePrice1 region = area region * totalPerimeter region

fencePrice2 :: Region -> Integer
fencePrice2 region = area region * totalSides region

area :: Set a -> Integer
area = toInteger . Set.size

totalPerimeter :: Region -> Integer
totalPerimeter region = sum $ map (toInteger . perimeterSides region) $ Set.toList region

perimeterSides :: Region -> Point2 Int -> Int
perimeterSides region = length . filter (\p -> not $ Set.member p region) . neighborsOf

-- Idea for part 2:
-- Each turn represents a side.
-- Start by filtering all the tiles that are part of the perimeter.
-- Start at some point at the perimeter, looking out from the region (upwards).
-- Move to the "right" until we encounter a turn. Increase turn counter by one.
-- Repeat until back at start position and start direction.
-- Remove all traversed tiles from perimeter set. If there are any left, pick a new random start point.
-- This last step is to account for the fact that we can have perimeters "on the inside" as well.

totalSides :: Region -> Integer
totalSides region = totalSides' region outsidePerimeter
  where
    outsidePerimeter = Set.unions $ Set.map (outsidePoints region) region

outsidePoints :: Region -> Point2 Int -> Set (Point2 Int)
outsidePoints region = Set.fromList . filter (\p -> not $ Set.member p region) . neighborsOf

totalSides' :: Region -> Set (Point2 Int) -> Integer
totalSides' region = go
  where
    go outsidePerimeter = case Set.toList outsidePerimeter of
      [] -> 0
      (point : _) -> let
        (sides, outsidePerimeter') = extractSides region outsidePerimeter point
        in sides + go outsidePerimeter'

extractSides :: Region -> Set (Point2 Int) -> Point2 Int -> (Integer, Set (Point2 Int))
extractSides region outsidePerimeter start = (sides, outsidePerimeter')
  where
    startDir = determineStartDir region start
    (sides, visitedOutsidePerimeter) = extractSides' region start startDir
    visitedOutsidePerimeter' = Set.map fst visitedOutsidePerimeter
    outsidePerimeter' = Set.difference outsidePerimeter visitedOutsidePerimeter'

determineStartDir :: Region -> Point2 Int -> Direction
determineStartDir region start = turnDirLeft direction
  where
    direction = fromJust $ find (\dir -> Set.member (moveByDir start dir) region) neighborDirections

extractSides' :: Region -> Point2 Int -> Direction -> (Integer, Set (Point2 Int, Direction))
extractSides' region start startDir = go 0 Set.empty start startDir
  where
    go sides visited point dir
      -- | traceShow (point, dir) (p2Y point < -50) = undefined
      | (nextPoint, nextDir) == (start, startDir) = (sides', visited')
      | otherwise = go sides' visited' nextPoint nextDir
      where
        (nextPoint, nextDir) = moveOne region visited point dir
        visited' = Set.insert (point, dir) visited
        sides' = sides + if dir == nextDir then 0 else 1

-- The idea is to walk along the outside, following the right wall.
-- We only move forward, and only if the right path is blocked and the forward path is free.
-- Basically, choose the first that applies:
-- 1) If right is free, we're turning around a corner, and have not been here with this direction before, turn right
-- 2) If forward is free, move forward
-- 3) Turn left
moveOne :: Region -> Set (Point2 Int, Direction) -> Point2 Int -> Direction -> (Point2 Int, Direction)
moveOne region visited point direction
  | canTurnRight region visited point direction = (point, rightDir)
  | not $ Set.member forwardPoint region = (forwardPoint, direction)
  | otherwise = (point, leftDir)
  where
    rightDir = turnDirRight direction
    leftDir = turnDirLeft direction
    forwardPoint = point `moveByDir` direction

canTurnRight :: Region -> Set (Point2 Int, Direction) -> Point2 Int -> Direction -> Bool
canTurnRight region visited point direction =
  isRightCorner region point direction &&
  not (Set.member (point, direction) visited)

isRightCorner :: Region -> Point2 Int -> Direction -> Bool
isRightCorner region point direction =
  Set.member rightBehindPoint region &&
  not (Set.member rightPoint region) &&
  not (Set.member behindPoint region)
  where
    rightDir = turnDirRight direction
    behindDir = turnDirRight rightDir
    rightBehindPoint = point `moveByDir` rightDir `moveByDir` behindDir
    rightPoint = point `moveByDir` rightDir
    behindPoint = point `moveByDir` behindDir

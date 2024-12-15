module Days.Day12 (solve) where
import           AoCUtils.Days     (Solver)
import           AoCUtils.Geometry (Point (moveBy), Point2, downV, leftV,
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
import           Debug.Trace       (traceShow, traceShowId)
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
solve2 = sum . map fencePrice2

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
extractSides region outsidePerimeter outsideStart = (sides, outsidePerimeter')
  where
    (start, startDir) = determineStart region outsideStart
    (sides, visitedPerimeter) = extractSides' outsidePerimeter start startDir
    outsidePerimeter' = traceShow visitedPerimeter $ Set.difference outsidePerimeter visitedPerimeter

determineStart :: Region -> Point2 Int -> (Point2 Int, Direction)
determineStart region outsidePoint = (perimeterPoint, moveDirection)
  where
    neighbors = zip (neighborsOf outsidePoint) neighborDirections
    (perimeterPoint, direction) = fromJust $ find (\(p, _) -> Set.member p region) neighbors
    moveDirection = turnDirLeft direction

extractSides' :: Set (Point2 Int) -> Point2 Int -> Direction -> (Integer, Set (Point2 Int))
extractSides' outsidePerimeter start startDir = go start startDir
  where
    go point dir
      | (nextPoint, nextDir) == (start, startDir) = (turnTerm, Set.singleton outsidePoint)
      | otherwise = let
        (sides, visited) = go nextPoint nextDir
        in (turnTerm + sides, Set.insert outsidePoint visited)
      where
        (nextPoint, nextDir) = moveOne outsidePerimeter point dir
        turnTerm = if dir == nextDir then 0 else 1
        outsidePoint = point `moveByDir` turnDirRight dir

-- Hug the left wall
moveOne :: Set (Point2 Int) -> Point2 Int -> Direction -> (Point2 Int, Direction)
moveOne outsidePerimeter point direction
  | Set.member left = undefined
  | Set.member forwardRight outsidePerimeter = (forward, direction)
  | otherwise = (forwardRight, turnDirRight direction)
  where
    left = point `moveByDir` turnDirLeft direction
    forwardRight = point `moveByDir` direction `moveByDir` turnDirRight direction
    forward = point `moveByDir` direction


module Days.Day12 (solve) where
import           AoCUtils.Days     (Solver)
import           AoCUtils.Geometry (Point (moveBy), Point2 (p2X, p2Y), downV,
                                    leftV, rightV, upV)
import           AoCUtils.Graphs   (BfsState (bfsPreMap), Goal (GFull),
                                    bfsExplore)
import           AoCUtils.Matrices (matrixToMapList)
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Maybe        (fromJust)
import           Data.Set          (Set)
import qualified Data.Set          as Set
import           Utils             (neighborsOf)

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
fencePrice2 region = area region * toInteger (totalSidesAlt region)

area :: Set a -> Integer
area = toInteger . Set.size

totalPerimeter :: Region -> Integer
totalPerimeter region = sum $ map (toInteger . perimeterSides region) $ Set.toList region

perimeterSides :: Region -> Point2 Int -> Int
perimeterSides region = length . filter (\p -> not $ Set.member p region) . neighborsOf

----------------------------------------------------------

data Side = Vertical Int (Int, Int) | Horizontal Int (Int, Int)
  deriving (Eq, Ord, Show)

-- TODO count sides, taking into account intersections
totalSidesAlt :: Region -> Int
totalSidesAlt region = sum $ map (subSides sides) $ Set.toList sides
  where
    sides = regionSides region

subSides :: Set Side -> Side -> Int
subSides sides side = 1 + length (Set.filter (intersect side) sides)

intersect :: Side -> Side -> Bool
intersect (Vertical x (yLow, yHigh)) (Horizontal y (xLow, xHigh)) =
  xLow < x && x < xHigh && yLow < y && y < yHigh
intersect h@(Horizontal _ _) v@(Vertical _ _) = intersect v h
intersect _ _                                 = False

regionSides :: Region -> Set Side
regionSides region = Set.fold mergeSide Set.empty singleSides
  where
    singleSides = Set.unions $ Set.map (tileSides region) region

tileSides :: Region -> Point2 Int -> Set Side
tileSides region point = Set.fromList $ map snd $ filter (\(p, _) -> not $ Set.member p region) neighborSides
  where
    x = p2X point
    y = p2Y point
    neighbors = neighborsOf point
    sides = [
      Horizontal y (x, x + 1),
      Vertical (x  + 1) (y, y + 1),
      Horizontal (y + 1) (x, x +1),
      Vertical x (y, y + 1)
      ]
    neighborSides = zip neighbors sides

mergeSide :: Side -> Set Side -> Set Side
mergeSide side acc = Set.insert side' acc'
  where
    connected = Set.filter (connectsWith side) acc
    acc' = Set.difference acc connected
    side' = combineSides connected side

combineSides :: Set Side -> Side -> Side
combineSides sides side = case side of
  Vertical x (yLow, yHigh)   -> case Set.toList sides of
    [] -> side
    [Vertical _ (y1Low, y1High)] -> Vertical x (min yLow y1Low, max yHigh y1High)
    [Vertical _ (y1Low, y1High), Vertical _ (y2Low, y2High)] -> Vertical x (min y1Low y2Low, max y1High y2High)
    _ -> error ("Invalid connections: " ++ show (sides, side))
  Horizontal y (xLow, xHigh) -> case Set.toList sides of
    [] -> side
    [Horizontal _ (x1Low, x1High)] -> Horizontal y (min xLow x1Low, max xHigh x1High)
    [Horizontal _ (x1Low, x1High), Horizontal _ (x2Low, x2High)] -> Horizontal y (min x1Low x2Low, max x1High x2High)
    _ -> error ("Invalid connections: " ++ show (sides, side))


connectsWith :: Side -> Side -> Bool
connectsWith (Vertical x1 (y1Low, y1High)) (Vertical x2 (y2Low, y2High)) =
  x1 == x2 && (y1Low == y2High || y1High == y2Low)
connectsWith (Horizontal y1 (x1Low, x1High)) (Horizontal y2 (x2Low, x2High)) =
  y1 == y2 && (x1Low == x2High || x1High == x2Low)
connectsWith _ _ = False

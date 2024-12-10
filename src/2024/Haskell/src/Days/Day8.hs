module Days.Day8 (solve) where
import           AoCUtils.Days     (Solver)
import           AoCUtils.Geometry (Point (moveBy, vectorBetween), Point2 (P2))
import           AoCUtils.Matrices (matrixToMapList)
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Set          (Set)
import qualified Data.Set          as Set

solve :: Solver
solve input = let
  (antennaMap, bounds) = parseAntennaMap input
  part1 = solve1 antennaMap bounds
  part2 = solve2 antennaMap bounds
  in (show part1, show part2)

type AntennaMap = Map Char [Point2 Int]

-- Inclusive
type Bounds = (Int, Int, Int, Int)

parseAntennaMap :: String -> (AntennaMap, Bounds)
parseAntennaMap str = (antennaMap, (0, maxX, 0, maxY))
  where
    (mapList, maxX, maxY) = matrixToMapList $ lines str
    antennaMap = foldr (uncurry insertAntenna) Map.empty mapList

insertAntenna :: Point2 Int -> Char -> AntennaMap -> AntennaMap
insertAntenna p c acc = case c of
  '.' -> acc
  _   -> Map.insertWith (++) c [p] acc

------------------------------------------------

solve1 :: AntennaMap -> Bounds -> Int
solve1 = antinodesWithinBounds

antinodesWithinBounds :: AntennaMap -> Bounds -> Int
antinodesWithinBounds antennaMap bounds =
  Set.size $ Set.filter (isWithin bounds) $ antinodes antennaMap

isWithin :: Bounds -> Point2 Int -> Bool
isWithin (minX, maxX, minY, maxY) (P2 x y) =
  x >= minX &&
  x <= maxX &&
  y >= minY &&
  y <= maxY

antinodes :: AntennaMap -> Set (Point2 Int)
antinodes = Set.unions . map (antinodesByAntenna . snd) . Map.toList

antinodesByAntenna :: [Point2 Int] -> Set (Point2 Int)
antinodesByAntenna antennas = Set.fromList $ [antinode x y | x <- antennas, y <- antennas, x /= y]

antinode :: Point2 Int -> Point2 Int -> Point2 Int
antinode p1 p2 = moveBy p2 diffV
  where
    diffV = vectorBetween p1 p2

solve2 :: AntennaMap -> Bounds -> Int
solve2 antennaMap = Set.size . antinodes2 antennaMap

antinodes2 :: AntennaMap -> Bounds -> Set (Point2 Int)
antinodes2 antennaMap bounds =
  Set.unions $ map (antinodesByAntenna2 bounds . snd) $ Map.toList antennaMap

antinodesByAntenna2 :: Bounds -> [Point2 Int] -> Set (Point2 Int)
antinodesByAntenna2 bounds antennas =
  Set.fromList $ concat [antinodesByPair bounds x y | x <- antennas, y <- antennas, x /= y]

-- Only gets the nodes in one direction, call in opposite order to get the rest.
antinodesByPair :: Bounds -> Point2 Int -> Point2 Int -> [Point2 Int]
antinodesByPair bounds p1 p2 = takeWhile (isWithin bounds) $ iterate (`moveBy` diffV) p2
  where
    diffV = vectorBetween p1 p2

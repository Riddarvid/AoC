{-# LANGUAGE TupleSections #-}
module Days.Day11 (solve) where
import           AoCUtils.Days     (Solver)
import           AoCUtils.Geometry (Point2 (p2Y))
import           AoCUtils.Matrices (matrixToHashMap)
import qualified Data.HashMap.Lazy as HM
import           Data.HashSet      (HashSet)
import qualified Data.HashSet      as HS
import           Data.List         (sort, transpose)

data Tile = TEmpty | TGalaxy
  deriving (Eq)

-- Since everyting is just a sum of Manhattan distances, we can compute distances in the
-- x and y dimensions separately and then add them together at the end.
solve :: Solver
solve input = let
  rows = parseInput input
  part1 = findDistanceSum 2 rows
  part2 = findDistanceSum 1000000 rows
  in (show part1, show part2)

parseInput :: String -> [[Tile]]
parseInput = map (map parseTile) . lines

parseTile :: Char -> Tile
parseTile '.' = TEmpty
parseTile '#' = TGalaxy
parseTile _   = error "Invalid character"

findDistanceSum :: Integer -> [[Tile]] -> Integer
findDistanceSum expFactor rows = let
  ySum = sum $ galaxyDistances expFactor rows
  xSum = sum $ galaxyDistances expFactor $ transpose rows
  in xSum + ySum

galaxyDistances :: Integer -> [[Tile]] -> [Integer]
galaxyDistances expFactor image = map (uncurry $ distance expFactor expandedRows) pairs
  where
    (galaxyMap, _, _) = matrixToHashMap image
    galaxyPoints = sort $ map p2Y $ HM.keys $ HM.filter (== TGalaxy) galaxyMap
    expandedRows = HS.fromList $ map fst $ filter (\(_, row) -> all (== TEmpty) row) $ zip [0 ..] image
    pairs = mkPairs galaxyPoints

mkPairs :: [a] -> [(a, a)]
mkPairs []       = []
mkPairs (x : xs) = map (x,) xs ++ mkPairs xs

-- We assume that p1 <= p2, since we sort in galaxyDistances
distance :: Integer -> HashSet Int -> Int -> Int -> Integer
distance expFactor emptyRows p1 p2 = baseDist + emptyRowsBetween * (expFactor - 1)
  where
    baseDist = toInteger $ p2 - p1
    emptyRowsBetween = toInteger $ length $ filter (`HS.member` emptyRows) [p1 .. p2]

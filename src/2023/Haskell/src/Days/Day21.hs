{-# LANGUAGE TupleSections #-}
module Days.Day21 (solve) where
import           AoCUtils.Days     (Solver)
import           AoCUtils.Geometry (Point2 (P2), downV, leftV, moveBy, rightV,
                                    upV)
import           AoCUtils.Matrices (matrixToHashMap)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import           Utils.Dijkstra    (exploreFully)

type Pos = Point2 Int

data Tile = Plot | Rock
  deriving (Eq)

solve :: Solver
solve input = let
  (start, tiles) = parseInput input
  part1 = solve1 start tiles
  in (show part1, "")

parseInput :: String -> (Pos, HashMap Pos Tile)
parseInput input = (start, HM.map parseTile chars)
  where
    (chars, _, _) = matrixToHashMap $ lines input
    start = head $ HM.keys $ HM.filter (== 'S') chars

parseTile :: Char -> Tile
parseTile 'S' = Plot
parseTile '.' = Plot
parseTile '#' = Rock
parseTile _   = error "Invalid tile"

solve1 :: Pos -> HashMap Pos Tile -> Int
solve1 start@(P2 startX startY) tiles = HM.size exactReachable
  where
    dists = exploreFully [start] (mkAdjacency tiles)
    reachable = HM.filter (<= 64) dists
    parity = (startX + startY) `mod` 2
    exactReachable = HM.filterWithKey (\(P2 x y) _ -> (x + y) `mod` 2 == parity) reachable

mkAdjacency :: HashMap Pos Tile -> Pos -> [(Pos, Int)]
mkAdjacency tiles p = map (, 1) plotNeighbors
  where
    neighbors = map (p `moveBy`) [upV, rightV, downV, leftV]
    plotNeighbors = filter isOpen neighbors
    isOpen :: Pos -> Bool
    isOpen p' = case HM.lookup p' tiles of
      Nothing   -> False
      Just tile -> tile == Plot

-- Part 2
-- We can assume that in a lot of these maps, we will be able to reach every square within them

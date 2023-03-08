{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Day6 (solve) where
import Solution (Solver)
import Parsing (getInts)

import Data.Set (Set)
import qualified Data.Set as Set
import Graphs (reachableBFSLimit, BFSOptions (BFSOptions, keepVisited, pruneFun))

type Pos = (Int, Int)

solve :: Solver
solve input = (show part1, "")
  where
    coords = map parseCoord $ lines input
    part1 = solve1 coords

-- Parsing

parseCoord :: String -> Pos
parseCoord str = (tokens !! 0, tokens !! 1)
  where
    tokens = getInts str

-- Part 1

solve1 :: [Pos] -> Int
solve1 coords = maximum $ filter (< limit) $ map (closestArea coords limit) coords
  where
    limit = findLimit coords

closestArea :: [Pos] -> Int -> Pos -> Int
closestArea coords limit current = Set.size $ reachableBFSLimit current limit adjacency options
  where
    options = BFSOptions {keepVisited = True, pruneFun = Nothing}
    adjacency = areaAdjecency coords current

findLimit :: [Pos] -> Int
findLimit _ = 5000 -- TODO fix

areaAdjecency :: [Pos] -> Pos -> Pos -> [Pos]
areaAdjecency coords current pos = filter (isCloserTo current coords) neighbors
  where
    neighbors = getNeighbors pos

getNeighbors :: Pos -> [Pos]
getNeighbors (x, y) = 
  [
    (x + 1, y), 
    (x - 1, y), 
    (x, y + 1), 
    (x, y - 1)]

isCloserTo :: Pos -> [Pos] -> Pos -> Bool
isCloserTo target coords pos = 
  all (\coord -> (coord == target) || (distance pos target < distance pos coord)) coords

distance :: Pos -> Pos -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

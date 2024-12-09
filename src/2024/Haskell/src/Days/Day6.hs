{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
module Days.Day6 (solve) where
import           AoCUtils.Days     (Solver)
import           AoCUtils.Geometry (Point (moveBy), Point2, downV, leftV,
                                    rightV, upV)
import           AoCUtils.Matrices (matrixToMapList)
import           Data.Foldable     (find)
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Maybe        (fromJust)
import           Data.Set          (Set)
import qualified Data.Set          as Set

data Tile = Clear | Blocked

solve :: Solver
solve input = let
  (tileMap, start) = parseTileMap input
  visited = findVisited tileMap start
  part1 = solve1 visited
  part2 = solve2 visited tileMap start
  in (show part1, show part2)

parseTileMap :: String -> (Map (Point2 Int) Tile, Point2 Int)
parseTileMap str = (charMap, start)
  where
    (mapList, _, _) = matrixToMapList $ lines str
    charMap = Map.map parseTile $ Map.fromList mapList
    start = fst $ fromJust $ find (\(_, c) -> c == '^') mapList

    parseTile '.' = Clear
    parseTile '^' = Clear
    parseTile '#' = Blocked
    parseTile _   = error "Illegal tile"

solve1 :: Set (Point2 Int) -> Int
solve1 = Set.size

solve2 :: Set (Point2 Int) -> Map (Point2 Int) Tile -> Point2 Int -> Int
solve2 candidates tileMap start = Set.size $ Set.filter (causesLoop tileMap start) candidates'
  where
    candidates' = Set.delete start candidates

causesLoop :: Map (Point2 Int) Tile -> Point2 Int -> Point2 Int -> Bool
causesLoop _tileMap start block = go Set.empty start DUp
  where
    tileMap = Map.insert block Blocked _tileMap
    go visited point direction = case Map.lookup point tileMap of
      Nothing -> False
      Just _ -> if Set.member (point, direction) visited
        then True
        else go visited' next nextDirection
      where
        visited' = Set.insert (point, direction) visited
        (next, nextDirection) = findNext tileMap point direction


data Direction = DUp | DRight | DDown | DLeft
  deriving (Eq, Ord)

findVisited ::
  Map (Point2 Int) Tile -> Point2 Int -> Set (Point2 Int)
findVisited tileMap start = go Set.empty start DUp
  where
    go visited point direction = case Map.lookup point tileMap of
      Nothing -> visited
      Just _  -> go visited' next nextDirection
      where
        visited' = Set.insert point visited
        (next, nextDirection) = findNext tileMap point direction

findNext :: Map (Point2 Int) Tile -> Point2 Int -> Direction -> (Point2 Int, Direction)
findNext tileMap = go
  where
    go point direction = case Map.lookup next tileMap of
      Just Blocked -> go point (turnRight direction)
      _            -> (next, direction)
      where
        nextV = case direction of
          DUp    -> upV
          DRight -> rightV
          DDown  -> downV
          DLeft  -> leftV
        next = moveBy point nextV

turnRight :: Direction -> Direction
turnRight DUp    = DRight
turnRight DRight = DDown
turnRight DDown  = DLeft
turnRight DLeft  = DUp

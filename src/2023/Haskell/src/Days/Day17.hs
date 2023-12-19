{-# LANGUAGE DeriveGeneric #-}
module Days.Day17 (solve) where
import           AoCUtils.Days     (Solver)
import           AoCUtils.Geometry (Point (moveBy), Point2 (P2), Vector2, downV,
                                    leftV, rightV, upV)
import           AoCUtils.Matrices (matrixToHashMap)
import           Data.Char         (digitToInt)
import           Data.Hashable     (Hashable)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import           Data.Maybe        (fromJust, mapMaybe)
import           GHC.Generics      (Generic)
import           Utils.Dijkstra    (distanceTo)

type Pos = Point2 Int

data Dir = DUp | DLeft | DDown | DRight
  deriving (Eq, Generic)

data State = State {
  sPos   :: Pos,
  sDir   :: Dir,
  sSteps :: Int
} deriving (Eq, Generic)

instance Hashable Dir

instance Hashable State

type WeightMap = HashMap Pos Int

solve :: Solver
solve input = let
  (weights, maxX, maxY) = matrixToHashMap $ map (map digitToInt) $ lines input
  part1 = solve1 weights maxX maxY
  part2 = solve2 weights maxX maxY
  in (show part1, show part2)

solve1 :: WeightMap -> Int -> Int -> Int
solve1 weights maxX maxY = fromJust $
  distanceTo [start] (\s -> sPos s == P2 maxX maxY) (getNeighbors weights)
  where
    start = State (P2 0 0) DDown 0

solve2 :: WeightMap -> Int -> Int -> Int
solve2 weights maxX maxY = fromJust $
  distanceTo starts (\s -> sPos s == P2 maxX maxY && sSteps s >= 4) (getUltraNeighbors weights)
  where
    starts = [State (P2 0 0) DDown 0, State (P2 0 0) DRight 0]

getNeighbors :: WeightMap -> State -> [(State, Int)]
getNeighbors weights s = mapMaybe (addCost weights) neighbors
  where
    f = stateForward s
    l = stateLeft s
    r = stateRight s
    neighbors = if sSteps f < 4
      then [f, l, r]
      else [l, r]

getUltraNeighbors :: WeightMap -> State -> [(State, Int)]
getUltraNeighbors weights s@(State _ _ steps) = mapMaybe (addCost weights) neighbors
  where
    turns = if steps >= 4
      then [stateLeft s, stateRight s]
      else []
    neighbors = if steps < 10
      then stateForward s : turns
      else turns


addCost :: WeightMap -> State -> Maybe (State, Int)
addCost weights s@(State pos _ _) = do
  weight <- HM.lookup pos weights
  return (s, weight)

stateForward :: State -> State
stateForward (State pos dir nSteps) = State (pos `moveBy` dirV dir) dir (nSteps + 1)

stateLeft :: State -> State
stateLeft (State pos dir _) = State (pos `moveBy` dirV dir') dir' 1
  where
    dir' = turnLeftDir dir

stateRight :: State -> State
stateRight (State pos dir _) = State (pos `moveBy` dirV dir') dir' 1
  where
    dir' = turnRightDir dir

turnLeftDir :: Dir -> Dir
turnLeftDir DUp    = DLeft
turnLeftDir DLeft  = DDown
turnLeftDir DDown  = DRight
turnLeftDir DRight = DUp

turnRightDir :: Dir -> Dir
turnRightDir = turnLeftDir . turnLeftDir . turnLeftDir

dirV :: Dir -> Vector2 Int
dirV DUp    = upV
dirV DLeft  = leftV
dirV DDown  = downV
dirV DRight = rightV

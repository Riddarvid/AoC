{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Days.Day14 (solve) where
import           AoCUtils.Days     (Solver)
import           AoCUtils.Geometry (Point (moveBy, scaleBy), Point2 (P2),
                                    Vector2)
import           AoCUtils.Regex    (parseSignedInts)
import           Data.List         (sortOn)
import qualified Data.Ord
import           Data.Set          (Set)
import qualified Data.Set          as Set

solve :: Solver
solve input = let
  robots = parseRobots input
  part1 = solve1 robots
  part2 = solve2 robots
  in (show part1, show part2)

data Robot = Robot (Point2 Int) (Vector2 Int)
  deriving (Eq)

parseRobots :: String -> [Robot]
parseRobots = map parseRobot . lines

parseRobot :: String -> Robot
parseRobot input = case parseSignedInts input of
  [x, y, dx, dy] -> Robot (P2 x y) (P2 dx dy)
  _              -> error "Invalid input"

------- Part 1 -------------

solve1 :: [Robot] -> Int
solve1 robots = safetyFactor width height robots'
  where
    width = 101
    height = 103
    moves = 100
    robots' = moveRobots moves width height robots

safetyFactor :: Int -> Int -> [Robot] -> Int
safetyFactor width height robots = q1 * q2 * q3 * q4
  where
    q1 = nRobotsInQuadrant Q1
    q2 = nRobotsInQuadrant Q2
    q3 = nRobotsInQuadrant Q3
    q4 = nRobotsInQuadrant Q4
    nRobotsInQuadrant = length . robotsInQuadrant
    robotsInQuadrant q = filter (inQuadrant q width height) robots

--------- Part 2 ---------------

solve2 :: [Robot] -> Int
solve2 robots = fst $ head sortedStates
  where
    width = 101
    height = 103
    states = zip [0 ..] $ robotStates width height robots
    sortedStates = sortOn (Data.Ord.Down . (\(_, state) -> closenessScore state)) states

showState :: Int -> Int -> (Int, [Robot]) -> String
showState width height (i, robots) = "Number of steps: " ++ show i ++ "\n" ++ showRobots width height robots

robotStates :: Int -> Int -> [Robot] -> [[Robot]]
robotStates width height robots = robots : states'
  where
    states = tail $ iterate (moveRobots 1 width height) robots
    states' = takeWhile (/= robots) states

showRobots :: Int -> Int -> [Robot] -> String
showRobots width height robots = unlines lines'
  where
    robotPoints = Set.fromList $ map (\(Robot p _) -> p) robots
    lines' = map (\y -> map (\x -> if Set.member (P2 x y) robotPoints then '#' else ' ') [0 .. width - 1]) [0 .. height - 1]

closenessScore :: [Robot] -> Int
closenessScore robots = sum $ map (nClose robotPoints) robots
  where
    robotPoints = Set.fromList $ map (\(Robot p _) -> p) robots

nClose :: Set (Point2 Int) -> Robot -> Int
nClose robotPoints (Robot (P2 x y) _) = length $ filter (`Set.member` robotPoints) closePoints
  where
    closePoints = [
      P2 (x - 1) (y - 1), P2 x (y - 1), P2 (x + 1) (y - 1),
      P2 (x - 1) y, P2 x y, P2 (x + 1) y,
      P2 (x - 1) (y + 1), P2 x (y + 1), P2 (x + 1) (y + 1)
      ]

-------- Common ----------------

moveRobots :: Int -> Int -> Int -> [Robot] -> [Robot]
moveRobots moves width height = map (moveRobot moves width height)

moveRobot :: Int -> Int -> Int -> Robot -> Robot
moveRobot moves width height (Robot p v) = Robot p' v
  where
    v' = scaleBy v moves
    (P2 x y) = moveBy p v'
    p' = P2 (x `mod` width) (y `mod` height)

data Quadrant = Q1 | Q2 | Q3 | Q4

inQuadrant :: Quadrant -> Int -> Int -> Robot -> Bool
inQuadrant q width height (Robot (P2 x y) _) =
  xMin <= x &&
  x <= xMax &&
  yMin <= y &&
  y <= yMax
  where
    (xMin, xMax, yMin, yMax) = case q of
      Q1 -> (xMid + 1, width - 1, 0, yMid - 1)
      Q2 -> (0, xMid - 1, 0, yMid - 1)
      Q3 -> (0, xMid - 1, yMid + 1, height - 1)
      Q4 -> (xMid + 1, width - 1, yMid + 1, height - 1)
    xMid = width `div` 2
    yMid = height `div` 2

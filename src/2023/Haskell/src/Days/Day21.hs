{-# LANGUAGE TupleSections #-}
module Days.Day21 (solve, Tile(..), solveGraphics) where
import           AoCUtils.Days     (Solver)
import           AoCUtils.Geometry (Point (scaleBy), Point2 (P2), downV, leftV,
                                    moveBy, rightV, upV)
import           AoCUtils.Matrices (matrixToHashMap)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import           Data.HashSet      (HashSet)
import qualified Data.HashSet      as HS
import           Data.Maybe        (fromJust)
import           Utils.Dijkstra    (exploreWithin)

type Pos = Point2 Int

data Tile = Plot | Rock
  deriving (Eq)

data Parity = Even | Odd
  deriving (Eq)

solve :: Solver
solve input = let
  (maxX, maxY, start, tiles) = parseInput input
  part1 = solve1 start tiles
  part2 = solve2 maxX maxY start tiles
  in (show part1, show part2)

parseInput :: String -> (Int, Int, Pos, HashMap Pos Tile)
parseInput input = (maxX, maxY, start, HM.map parseTile chars)
  where
    (chars, maxX, maxY) = matrixToHashMap $ lines input
    start = head $ HM.keys $ HM.filter (== 'S') chars

parseTile :: Char -> Tile
parseTile 'S' = Plot
parseTile '.' = Plot
parseTile '#' = Rock
parseTile _   = error "Invalid tile"

solve1 :: Pos -> HashMap Pos Tile -> Int
solve1 = nExactReachableSearch Even (64 :: Int)

nExactReachableSearch :: Integral a => Parity -> a -> Pos -> HashMap Pos Tile -> Int
nExactReachableSearch parity steps start = HS.size . findExactReachableSearch parity steps start

findExactReachableSearch :: Integral a => Parity -> a -> Pos -> HashMap Pos Tile -> HashSet Pos
findExactReachableSearch parity steps start tiles = exactReachable
  where
    dists = exploreWithin steps [start] (mkAdjacency tiles)
    reachable = HM.keysSet $ HM.filter (\d -> toInteger d <= toInteger steps) dists
    exactReachable = HS.filter (isExactReachable parity start) reachable

isExactReachable :: Parity -> Pos -> Pos -> Bool
isExactReachable parity (P2 startX startY) (P2 x y) = (x + y) `mod` 2 == parityBit
  where
    parityBit = if parity == Odd
      then (startX + startY + 1) `mod` 2
      else (startX + startY) `mod` 2

-- Imporvement: In dijkstra, add condition for stopping when distance becomes too high
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

solve2 :: Int -> Int -> Pos -> HashMap Pos Tile -> Integer
solve2 maxX maxY start = fromJust . solveGeneral maxX maxY 26501365 start

-- Checks that the input is a square, that the starting position is in the middle of it,
-- and that the steps value can be expressed as n * 2 * side + mid
solveGeneral :: Int -> Int -> Integer -> Pos -> HashMap Pos Tile -> Maybe Integer
solveGeneral maxX maxY steps start tiles
  | maxX /= maxY = Nothing
  | even side = Nothing
  | start /= P2 mid mid = Nothing
  | (steps - toInteger mid) `mod` (2 * toInteger side) /= 0 = Nothing
  | otherwise = Just $ solveGeneral' side steps tiles
  where
    side = maxX + 1 -- 0 indexing
    mid = (side - 1) `div` 2

solveGeneral' :: Int -> Integer -> HashMap Pos Tile -> Integer
solveGeneral' side steps tiles = a * na + a' * na' + bb' * nbb'
  where
    mid = (side - 1) `div` 2
    start = P2 mid mid
    a = toInteger $ nExactReachableSearch Odd mid start tiles
    a' = toInteger $ nExactReachableSearch Even (mid - 1) start tiles
    aa'bb' = toInteger $ nExactReachableSearch Odd steps start tiles +
      nExactReachableSearch Even steps start tiles
    bb' = aa'bb' - a - a'

    n = (steps - toInteger mid) `div` (2 * toInteger side)
    na = (2 * n + 1) ^ (2 :: Integer)
    na' = (2 * n) ^ (2 :: Integer)
    nbb' = ((4 * n + 1) ^ (2 :: Integer) - na - na') `div` 2

-- Graphics

solveGraphics :: String -> (Int, Int, HashMap Pos Tile, HashSet Pos)
solveGraphics input = (maxX, maxY, tiles, exactReachable)
  where
    (maxX, maxY, start, tiles) = scaleMap 5 input
    exactReachable = findExactReachableSearch Odd (65 + 131 * 2 :: Int) start tiles

-- Factor must be odd for start position scaling to work
scaleMap :: Int -> String -> (Int, Int, Pos, HashMap Pos Tile)
scaleMap factor input = (maxX, maxY, startScaled, HM.map parseTile scaledChars)
  where
    baseGrid = lines input
    row = map (concat . replicate factor) baseGrid
    scaledGrid = concat $ replicate factor row
    (scaledChars, maxX, maxY) = matrixToHashMap scaledGrid
    (chars, _, _) = matrixToHashMap $ lines input
    start = head $ HM.keys $ HM.filter (== 'S') chars
    startScaled = start `moveBy`
      (rightV `scaleBy` (length (head (lines input)) * (factor `div` 2)))
      `moveBy`
      (downV `scaleBy` (length (lines input) * (factor `div` 2)))

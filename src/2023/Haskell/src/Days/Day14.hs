{-# LANGUAGE NumericUnderscores #-}
module Days.Day14 (solve) where
import           AoCUtils.Days     (Solver)
import           AoCUtils.Geometry (Point (moveBy, scaleBy), Point2 (P2),
                                    Vector2, downV, leftV, rightV, upV)
import           AoCUtils.Matrices (matrixToHashMap)
import           Data.Hashable     (Hashable)
import qualified Data.HashMap.Lazy as HM
import           Data.HashSet      (HashSet)
import qualified Data.HashSet      as HS

type Pos = Point2 Int

solve :: Solver
solve input = let
  (rocks, maxX, maxY) = matrixToHashMap $ lines input
  cubes = HM.keysSet $ HM.filter (== '#') rocks
  rounds = HM.keysSet $ HM.filter (== 'O') rocks
  part1 = solve1 maxX maxY cubes rounds
  part2 = solve2 maxX maxY cubes rounds
  in (show part1, show part2)

solve1 :: Int -> Int -> HashSet Pos -> HashSet Pos -> Integer
solve1 maxX maxY cubes rounds = calculateLoad maxY rounds'
  where
    rounds' = slide upV maxX maxY cubes rounds

slide :: Vector2 Int -> Int -> Int -> HashSet Pos -> HashSet Pos -> HashSet Pos
slide dir maxX maxY cubes rounds = HS.map (moveRock dir maxX maxY cubes rounds) rounds

moveRock :: Vector2 Int -> Int -> Int -> HashSet Pos -> HashSet Pos -> Pos -> Pos
moveRock dir maxX maxY cubes rounds p = p'
  where
    p' = p `moveBy` (dir `scaleBy` distance)
    distance = findSlideDistance dir maxX maxY cubes rounds p

findSlideDistance :: Vector2 Int -> Int -> Int -> HashSet Pos -> HashSet Pos -> Pos -> Int
findSlideDistance dir maxX maxY cubes rounds = go
  where
    go :: Pos -> Int
    go p@(P2 x y)
      | x < 0 || y < 0 || x > maxX || y > maxY = 0
      | p `HS.member` cubes = 0
      | otherwise = let
        d = if p `HS.member` rounds then 0 else 1
        in d + go (p `moveBy` dir)

calculateLoad :: Int -> HashSet Pos -> Integer
calculateLoad maxY = sum . map (\(P2 _ y) -> toInteger $ (maxY - y) + 1) . HS.toList

solve2 :: Int -> Int -> HashSet Pos -> HashSet Pos -> Integer
solve2 maxX maxY cubes rounds = calculateLoad maxY (cycle' !! cycleIndex)
  where
    offset = length $ takeWhileUnique $ iterate (execSpinCycle maxX maxY cubes) rounds
    cycle' = takeWhileUnique $ drop offset $ iterate (execSpinCycle maxX maxY cubes) rounds
    cycleLength = length cycle'
    cycleIndex = (1_000_000_000 - offset) `mod` cycleLength

execSpinCycle :: Int -> Int -> HashSet Pos -> HashSet Pos -> HashSet Pos
execSpinCycle maxX maxY cubes rounds =
  foldl (\rounds' dir -> slide dir maxX maxY cubes rounds') rounds [upV, leftV, downV, rightV]

takeWhileUnique :: Hashable a => [a] -> [a]
takeWhileUnique = takeWhileUnique' HS.empty

takeWhileUnique' :: Hashable a => HashSet a -> [a] -> [a]
takeWhileUnique' _ [] = []
takeWhileUnique' seen (x : xs)
  | x `HS.member` seen = []
  | otherwise = x : takeWhileUnique' (HS.insert x seen) xs

showRocks :: Int -> Int -> HashSet Pos -> HashSet Pos -> String
showRocks maxX maxY cubes rounds = unlines $
  map (\y -> map (\x -> toChar $ P2 x y) [0 .. maxX]) [0 .. maxY]
  where
    toChar :: Pos -> Char
    toChar p
      | p `HS.member` cubes = '#'
      | p `HS.member` rounds = 'O'
      | otherwise = '.'

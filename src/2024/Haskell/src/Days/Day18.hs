module Days.Day18 (solve) where
import           AoCUtils.Days     (Solver)
import           AoCUtils.Geometry (Point2 (P2))
import           AoCUtils.Graphs   (Goal (GTarget), bfsPath)
import           AoCUtils.Regex    (parseSignedInts)
import           Data.Foldable     (find)
import           Data.List         (inits)
import           Data.Maybe        (fromJust, isNothing)
import qualified Data.Set          as Set
import           Utils             (neighborsOf)

solve :: Solver
solve input = let
  corrupted = parseInput input
  part1 = solve1 corrupted
  part2 = solve2 corrupted
  in (show part1, part2)

parseInput :: String -> [Point2 Int]
parseInput = map parsePoint . lines

parsePoint :: String -> Point2 Int
parsePoint line = case parseSignedInts line of
  [x, y] -> P2 x y
  _      -> error "Invalid point"

solve1 :: [Point2 Int] -> Int
solve1 corrupted = length path - 1
  where
    path = fromJust $ findPath $ take 1024 corrupted

solve2 :: [Point2 Int] -> String
solve2 corrupted = show x ++ "," ++ show y
  where
    corrupteds = inits corrupted
    firstBlocking = fromJust $ find (isNothing . findPath) corrupteds
    P2 x y = last firstBlocking

findPath :: [Point2 Int] -> Maybe [Point2 Int]
findPath corrupted = bfsPath (P2 0 0) (GTarget (P2 70 70)) (neighborsFun corrupted)

neighborsFun :: [Point2 Int] -> Point2 Int -> [Point2 Int]
neighborsFun corrupted p = neighbors''
  where
    corruptMap = Set.fromList corrupted
    neighbors = neighborsOf p
    neighbors' = filter isWithinBounds neighbors
    neighbors'' = filter (\n -> not $ Set.member n corruptMap) neighbors'

isWithinBounds :: Point2 Int -> Bool
isWithinBounds (P2 x y) =
  x >= 0 &&
  x <= 70 &&
  y >= 0 &&
  y <= 70

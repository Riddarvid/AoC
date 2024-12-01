module Days.Day1 (solve) where
import           AoCUtils.Days   (Solver)
import           AoCUtils.Regex  (parseUnsignedInts)
import           Data.List       (sort, transpose)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

solve :: Solver
solve input = let
  lists = parseLists input
  part1 = uncurry solve1 lists
  part2 = uncurry solve2 lists
  in (show part1, show part2)

parseLists :: String -> ([Integer], [Integer])
parseLists input = case transpose rows of
  [left, right] -> (left, right)
  _             -> error "Invalid number of lists"
  where
    rows = map parseUnsignedInts $ lines input

solve1 :: [Integer] -> [Integer] -> Integer
solve1 left right = sum $ zipWith (\a b -> abs (a - b)) left' right'
  where
    left' = sort left
    right' = sort right

solve2 :: [Integer] -> [Integer] -> Integer
solve2 left right = sum $ map (\n -> n * Map.findWithDefault 0 n occurences) left
  where
    occurences = buildOccurenceMap right

buildOccurenceMap :: Ord a => [a] -> Map a Integer
buildOccurenceMap = foldr (\x -> Map.insertWith (+) x 1) Map.empty

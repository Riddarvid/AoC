module Days.Day09 (solve) where
import           AoCUtils.Days  (Solver)
import           AoCUtils.Regex (parseSignedInts)

solve :: Solver
solve input = let
  histories = map parseSignedInts $ lines input
  part1 = sum $ map extrapolateLast histories
  part2 = sum $ map extrapolateHead histories
  in (show part1, show part2)

extrapolateLast :: [Integer] -> Integer
extrapolateLast vals
  | all (== 0) vals = 0
  | otherwise = last' + diff
  where
    last' = last vals
    diff = extrapolateLast $ steps vals

extrapolateHead :: [Integer] -> Integer
extrapolateHead vals
  | all (== 0) vals = 0
  | otherwise = head' - diff
  where
    head' = head vals
    diff = extrapolateHead $ steps vals

steps :: [Integer] -> [Integer]
steps (a : b : xs) = b - a : steps (b : xs)
steps _            = []

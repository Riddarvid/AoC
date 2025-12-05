module Days.Day5 (solve) where
import           AoCUtils.Days (Solver)

data Range = Range Integer Integer
  deriving (Show)

solve :: Solver
solve input = let
  (freshRanges, ids) = parseInput input
  part1 = solve1 freshRanges ids
  part2 = solve2 freshRanges
  in (show part1, show part2)

parseInput :: String -> ([Range], [Integer])
parseInput input = (ranges, ids)
  where
    (rangeLines, input') = span (/= "") $ lines input
    ranges = map parseRange rangeLines
    idLines = tail input'
    ids = map read idLines

parseRange :: String -> Range
parseRange input = Range (read firstIntString) (read secondIntString)
  where
    (firstIntString, input') = span (/= '-') input
    secondIntString = tail input'

solve1 :: [Range] -> [Integer] -> Int
solve1 ranges = length . filter (\i -> any (isInRange i) ranges)

isInRange :: Integer -> Range -> Bool
isInRange i (Range low high) = low <= i && i <= high

solve2 :: [Range] -> Integer
solve2 = sum . map rangeSize . mergeAllRanges

rangeSize :: Range -> Integer
rangeSize (Range low high) = high - low + 1

mergeAllRanges :: [Range] -> [Range]
mergeAllRanges = foldr mergeRangeWithAll []

mergeRangeWithAll :: Range -> [Range] -> [Range]
mergeRangeWithAll toMerge merged = case popFirstIntersecting toMerge merged of
  Nothing -> toMerge : merged
  Just (intersecting, merged') ->
    mergeRangeWithAll (mergeRanges toMerge intersecting) merged'

popFirstIntersecting :: Range -> [Range] -> Maybe (Range, [Range])
popFirstIntersecting _ [] = Nothing
popFirstIntersecting range (other : ranges)
  | range `intersects` other = Just (other, ranges)
  | otherwise = do
    (intersecting, ranges') <- popFirstIntersecting range ranges
    return (intersecting, other : ranges')

intersects :: Range -> Range -> Bool
intersects (Range low1 high1) (Range low2 high2) =
  low1 <= high2 && high1 >= low2

mergeRanges :: Range -> Range -> Range
mergeRanges (Range low1 high1) (Range low2 high2) = Range (min low1 low2) (max high1 high2)

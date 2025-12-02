module Days.Day2 (solve) where
import           AoCUtils.Days (Solver)
import           Utils         (chunks, split)

data Range = Range Integer Integer
  deriving (Show)

solve :: Solver
solve input = let
  ranges = parseRanges (head $ lines input)
  part1 = solve1 ranges
  part2 = solve2 ranges
  in (show part1, show part2)

parseRanges :: String -> [Range]
parseRanges = map parseRange . split ','

parseRange :: String -> Range
parseRange input = case split '-' input of
  [start, end] -> Range (read start) (read end)
  _            -> error "Couldn't parse range"

-- General

solveGeneral :: (Integer -> Bool) -> [Range] -> Integer
solveGeneral isInvalidFun ranges = sum invalidIds
  where
    invalidIds = concatMap (filterInvalidIds isInvalidFun) ranges

filterInvalidIds :: (Integer -> Bool) -> Range -> [Integer]
filterInvalidIds isInvalidFun (Range low high) = filter isInvalidFun [low .. high]

-- Part1

solve1 :: [Range] -> Integer
solve1 = solveGeneral isInvalidId1

isInvalidId1 :: Integer -> Bool
isInvalidId1 n
  | odd (length nString) = False
  | otherwise = isInvalidByRepLength nString (length nString `div` 2)
  where
    nString = show n

-- Part2

solve2 :: [Range] -> Integer
solve2 = solveGeneral isInvalidId2

-- Based on length, create the list of possible lengths [1, 2, ..., length / 2]
-- Check for all lengths
isInvalidId2 :: Integer -> Bool
isInvalidId2 n = any (isInvalidByRepLength nString) [1 .. nLength `div` 2]
  where
    nString = show n
    nLength = length nString

isInvalidByRepLength :: String -> Int -> Bool
isInvalidByRepLength nString repLength
  | length nString `mod` repLength /= 0 = False
  | otherwise = all (== firstChunk) nChunks
  where
    nChunks = chunks repLength nString
    firstChunk = head nChunks

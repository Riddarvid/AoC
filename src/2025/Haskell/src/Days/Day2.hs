{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Days.Day2 (solve, propSameInvalids1, propSameInvalids2) where
import           AoCUtils.Days   (Solver)
import           Data.Foldable   (find)
import           Data.List       (sort)
import           Data.Maybe      (fromJust)
import           Test.QuickCheck (Arbitrary (arbitrary), Gen, Property, (===),
                                  (==>))
import           Utils           (chunks, split)

data Range = Range Integer Integer
  deriving (Show)

instance Arbitrary Range where
  arbitrary :: Gen Range
  arbitrary = Range <$> arbitrary <*> arbitrary

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

solveGeneral :: (Integer -> [Integer]) -> [Range] -> Integer
solveGeneral possibleNextFun = sum . concatMap (generateInvalidIds possibleNextFun)

generateInvalidIds :: (Integer -> [Integer]) -> Range -> [Integer]
generateInvalidIds possibleNextFun (Range low high) =
  takeWhile (<= high) invalidSequence
  where
    invalidSequence = tail $ iterate (generateNextInvalid possibleNextFun) (low - 1)

generateNextInvalid :: (Integer -> [Integer]) -> Integer -> Integer
generateNextInvalid possibleNextFun n = fromJust $ find (> n) $ sort possibleNextInvalids
  where
    possibleNextInvalids = possibleNextFun n

-- Part 1

solve1 :: [Range] -> Integer
solve1 = solveGeneral generatePossibleNextInvalids1

generatePossibleNextInvalids1 :: Integer -> [Integer]
generatePossibleNextInvalids1 n = possibleForCurrentLength ++ possibleForNextLength
  where
    nString = show n
    nLength = length nString
    nextMagnitudeString = '1' : replicate nLength '0'
    possibleForCurrentLength = map read $ generatePossibleNextInvalids1' nString
    possibleForNextLength = map read $ generatePossibleNextInvalids1' nextMagnitudeString

generatePossibleNextInvalids1' :: String -> [String]
generatePossibleNextInvalids1' nString = findPossibleNextInvalidByRepLength nString repLength
  where
    nLength = length nString
    repLength = if odd nLength then (nLength `div` 2) + 1 else nLength `div` 2

-- Part 2

solve2 :: [Range] -> Integer
solve2 = solveGeneral generatePossibleNextInvalids2

-- Note that for any number x of length n, if there are no invalid IDs > x of length n,
-- Then the next invalid ID will be greater than the number formed by taking
-- 1 followed by n zeroes. It is also guaranteed that the next invalid ID will have
-- a length of exactly n + 1 (for example all 1's).
generatePossibleNextInvalids2 :: Integer -> [Integer]
generatePossibleNextInvalids2 n = possibleForCurrentLength ++ possibleForNextLength
  where
    nString = show n
    nLength = length nString
    nextMagnitudeString = '1' : replicate nLength '0'
    possibleForCurrentLength = map read $ generatePossibleNextInvalids2' nString
    possibleForNextLength = map read $ generatePossibleNextInvalids2' nextMagnitudeString

generatePossibleNextInvalids2' :: String -> [String]
generatePossibleNextInvalids2' nString = concatMap (findPossibleNextInvalidByRepLength nString) [1 .. repLength]
  where
    nLength = length nString
    repLength = if odd nLength then (nLength `div` 2) + 1 else nLength `div` 2

-- General

findPossibleNextInvalidByRepLength :: String -> Int -> [String]
findPossibleNextInvalidByRepLength nString repLength
  | remainder /= 0 = []
  | nLength == repLength = []
  | otherwise = [
    concat (replicate nSegments repSegment),
    concat (replicate nSegments incdRepSegment)
  ]
  where
    nLength = length nString
    remainder = nLength `mod` repLength
    repSegment = take repLength nString
    incdRepSegment = show (read repSegment + 1 :: Integer)
    nSegments = nLength `div` repLength

-- Navie slow method

solveGeneralNaive :: (Integer -> Bool) -> [Range] -> Integer
solveGeneralNaive isInvalidFun ranges = sum invalidIds
  where
    invalidIds = concatMap (filterInvalidIdsNaive isInvalidFun) ranges

filterInvalidIdsNaive :: (Integer -> Bool) -> Range -> [Integer]
filterInvalidIdsNaive isInvalidFun (Range low high) = filter isInvalidFun [low .. high]

-- Part1 naive

solve1Naive :: [Range] -> Integer
solve1Naive = solveGeneralNaive isInvalidId1Naive

isInvalidId1Naive :: Integer -> Bool
isInvalidId1Naive n
  | odd (length nString) = False
  | otherwise = isInvalidByRepLengthNaive nString (length nString `div` 2)
  where
    nString = show n

-- Part2 naive

solve2Naive :: [Range] -> Integer
solve2Naive = solveGeneralNaive isInvalidId2Naive

-- Based on length, create the list of possible lengths [1, 2, ..., length / 2]
-- Check for all lengths
isInvalidId2Naive :: Integer -> Bool
isInvalidId2Naive n = any (isInvalidByRepLengthNaive nString) [1 .. nLength `div` 2]
  where
    nString = show n
    nLength = length nString

isInvalidByRepLengthNaive :: String -> Int -> Bool
isInvalidByRepLengthNaive nString repLength
  | length nString `mod` repLength /= 0 = False
  | otherwise = all (== firstChunk) nChunks
  where
    nChunks = chunks repLength nString
    firstChunk = head nChunks

-- Properties

propSameInvalids1 :: Range -> Property
propSameInvalids1 range@(Range low high) = low < high && low > 0 ==>
  filterInvalidIdsNaive isInvalidId1Naive range ===
  generateInvalidIds generatePossibleNextInvalids1 range

propSameInvalids2 :: Range -> Property
propSameInvalids2 range@(Range low high) = low < high && low > 0 ==>
  filterInvalidIdsNaive isInvalidId2Naive range ===
  generateInvalidIds generatePossibleNextInvalids2 range

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Days.Day19 (solve) where
import           AoCUtils.Days         (Solver)
import           Data.Function         (on)
import           Data.Function.Memoize (Memoizable (memoize))
import           Data.List             (groupBy, inits, tails)
import           Data.Set              (Set)
import qualified Data.Set              as Set

-- I think it would make sens to use a trie or similar to represent
-- the available towels. Right now we will instead use a set of towels
-- and keep track of the max size of the towels, and therefore only check up
-- to that many characters ahead.
solve :: Solver
solve input = let
  (towels, designs) = parseInput input
  constructions = waysToConstructs towels designs
  part1 = solve1 constructions
  part2 = solve2 constructions
  in (show part1, show part2)

parseInput :: String -> ([String], [String])
parseInput input = (towels, designs)
  where
    rows = lines input
    towels = filter (/= ", ") $ groupBy (on (==) (`elem` [',', ' '])) $ head rows
    designs = drop 2 rows

solve1 :: [Integer] -> Int
solve1 = length . filter (> 0)

solve2 :: [Integer] -> Integer
solve2 = sum

constructDesigns :: [String] -> [String] -> [[[String]]]
constructDesigns towels = map (constructDesign towelSet maxSize)
  where
    towelSet = Set.fromList towels
    maxSize = maximum $ map length towels

-- I like this approach, but it uses too much memory. Let's do another
-- version which only counts the number of ways to construct a design.
-- The problem it that the memoized structures are long lists,
-- which of course uses a lot more memory than an int and a string per entry.
constructDesign :: Set String -> Int -> String -> [[String]]
constructDesign towelSet maxTowelLength = memoGo
  where
    memoGo = memoize go
    go [] = [[]]
    go design = concatMap (uncurry appendTowel) viableCandidates
      where
        prefixes = take maxTowelLength $ tail $ inits design
        suffixes = tail $ tails design
        prefixDesigns = zip prefixes suffixes
        viableCandidates = filter (\(pre, _) -> pre `Set.member` towelSet) prefixDesigns
        appendTowel towel design' = map (towel :) $ go design'

waysToConstructs :: [String] -> [String] -> [Integer]
waysToConstructs towels = map (waysToConstruct towelSet maxTowelLength)
  where
    towelSet = Set.fromList towels
    maxTowelLength = maximum $ map length towels

waysToConstruct :: Set String -> Int -> String -> Integer
waysToConstruct towelSet maxTowelLength = memoGo
  where
    memoGo = memoize go
    go [] = 1
    go design = sum $ map memoGo viableSuffixes
      where
        prefixes = take maxTowelLength $ tail $ inits design
        suffixes = tail $ tails design
        prefixDesigns = zip prefixes suffixes
        viableCandidates = filter (\(pre, _) -> pre `Set.member` towelSet) prefixDesigns
        viableSuffixes = map snd viableCandidates

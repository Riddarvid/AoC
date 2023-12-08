{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Days.Day8 (solve) where
import           AoCUtils.Days      (Solver)
import           Data.HashMap.Lazy  (HashMap)
import qualified Data.HashMap.Lazy  as HM
import           Data.List          (singleton)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import           Text.Regex.PCRE    (AllTextMatches (getAllTextMatches), (=~))

type DesertMap = HashMap String (String, String)

data Direction = L | R
  deriving (Show, Read)

type Directions = [Direction]

solve :: Solver
solve input = let
  (directions, desertMap) = parseInput input
  part1 = solve1 directions desertMap
  part2 = solve2 directions desertMap
  in (show part1, show part2)

parseInput :: String -> (Directions, DesertMap)
parseInput input = (directions, desertMap)
  where
    lines' = lines input
    directions = map (read . singleton) $ head lines'
    desertMap = HM.fromList $ map parseLine $ drop 2 lines'

parseLine :: String -> (String, (String, String))
parseLine line = (tokens !! 0, (tokens !! 1, tokens !! 2))
  where
    tokens = getAllTextMatches (line =~ "[A-Z]+")

-- General

move :: DesertMap -> String -> NonEmpty Direction -> [String]
move desertMap pos (dir :| dirs) = case HM.lookup pos desertMap of
  Nothing -> error "Pos not in map"
  Just (l, r) -> let
    pos' = case dir of
      L -> l
      R -> r
    in pos : move desertMap pos' (NE.fromList dirs)

shortestDistance :: Directions -> DesertMap -> [String] -> (String -> Bool) -> Integer
shortestDistance directions desertMap starts endCond = foldr lcm 1 cycles
  where
    paths = map (\s -> move desertMap s (NE.fromList $ cycle directions)) starts
    cycles = map (head . map fst . filter (\(_, pos) -> endCond pos) . zip [0 .. ]) paths

-- Part 1

solve1 :: Directions -> DesertMap -> Integer
solve1 directions desertMap =
  shortestDistance directions desertMap ["AAA"] (== "ZZZ")

-- Part 2

-- We only have 6 separate starts
-- Assumption after looking at the data: The distance to the first end will always be the
-- cycle length
solve2 :: Directions -> DesertMap -> Integer
solve2 directions desertMap =
  shortestDistance directions desertMap starts (\s -> last s == 'Z')
  where
    starts = filter ((== 'A') . last) $ HM.keys desertMap

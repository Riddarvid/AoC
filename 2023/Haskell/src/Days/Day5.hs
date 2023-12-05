{-# LANGUAGE InstanceSigs #-}
module Days.Day5 (solve) where
import           AoCUtils.Days  (Solver)
import           AoCUtils.Regex (parseUnsignedInts)
import           Data.Foldable  (Foldable (foldl'))
import           Data.Function  (on)
import           Data.List      (groupBy, sort)
import           Data.Maybe     (mapMaybe)

solve :: Solver
solve input = let
  (seeds, maps) = parseInput input
  part1 = solve1 seeds maps
  part2 = solve2 seeds maps
  in (show part1, show part2)

newtype Map = Map [MapEntry]

-- Dest start, src start, length
data MapEntry = Entry {
  _eDestStart :: Integer,
  eSrcStart   :: Integer,
  _eLength    :: Integer
} deriving (Eq)

instance Ord MapEntry where
  (<=) :: MapEntry -> MapEntry -> Bool
  (<=) = (<=) `on` eSrcStart

type Seed = Integer

-- Parsing

parseInput :: String -> ([Seed], [Map])
parseInput input = (seeds, maps)
  where
    chunks = filter (/= [""]) $ groupBy ((==) `on` null) $ lines input
    seeds = parseUnsignedInts $ head $ head chunks
    maps = map parseMap $ tail chunks

parseMap :: [String] -> Map
parseMap = Map . sort . map parseEntry . tail

parseEntry :: String -> MapEntry
parseEntry line = case parseUnsignedInts line of
  [destStart, srcStart, entryLength] -> Entry destStart srcStart entryLength
  _ -> error $ "Expected three numbers, got: " ++ line

-- Solving

-- Assumption: the maps are listed in the correct order
solve1 :: [Seed] -> [Map] -> Integer
solve1 seeds maps = minimum $ map (toLocation maps) seeds

toLocation :: [Map] -> Seed -> Integer
toLocation maps seed = foldl' useMap seed maps

-- Assumption: a number is a member of at most one range
useMap :: Integer -> Map -> Integer
useMap input (Map entries) = case mapMaybe (tryMap input) entries of
  []       -> input
  [output] -> output
  _        -> error "a number should be a member of at most one range"

tryMap :: Integer -> MapEntry -> Maybe Integer
tryMap input (Entry destStart srcStart rangeLength)
  | srcStart <= input && input < srcStart + rangeLength = Just (destStart + diff)
  | otherwise = Nothing
  where
    diff = input - srcStart

-- Part 2

data Range = Range Integer Integer
  deriving (Eq)

rFst :: Range -> Integer
rFst (Range x _) = x

rPoints :: MapEntry -> [Integer]
rPoints (Entry _ start length') = [start, start + length']

instance Ord Range where
  (<=) :: Range -> Range -> Bool
  (<=) = (<=) `on` rFst

solve2 :: [Seed] -> [Map] -> Integer
solve2 seeds maps =
  rFst $ minimum $ foldl' (\ranges map' -> concatMap (mapRange map') ranges) seedRanges maps
  where
    seedRanges = parseRanges seeds

parseRanges :: [Seed] -> [Range]
parseRanges [] = []
parseRanges (start : length' : xs) = Range start (start + length') : parseRanges xs
parseRanges _ = error "Invalid seeds"

mapRange :: Map -> Range -> [Range]
mapRange map'@(Map entries) (Range start end) = map (mapRange' map') ranges
  where
    -- Sorted since the entries are sorted
    extremePoints' = concatMap (filter (\x -> start <= x && x < end) . rPoints) entries
    extremePoints = start : extremePoints' ++ [end]
    ranges = pointsToRanges extremePoints

pointsToRanges :: [Integer] -> [Range]
pointsToRanges []                 = []
pointsToRanges [_]                = []
pointsToRanges (start : end : xs) = Range start end : pointsToRanges (end : xs)

-- Assumes that all the points in the input range are covered by a contigious segment in the map
mapRange' :: Map -> Range -> Range
mapRange' map' (Range start end) = Range start' (end + diff)
  where
    start' = useMap start map'
    diff = start' - start



{-# OPTIONS_GHC -Wno-type-defaults #-}
module Days.Day6 (solve) where
import           AoCUtils.Days  (Solver)
import           AoCUtils.Regex (parseUnsignedInts)

solve :: Solver
solve input = let
  races = parseRaces input
  part1 = solutionProducts races
  race = parseRace input
  part2 = solutionProducts [race]
  in (show part1, show part2)

data Race = Race Integer Integer

parseRaces :: String -> [Race]
parseRaces input = zipWith Race times distances
  where
    lines' = lines input
    times = parseUnsignedInts $ head lines'
    distances = parseUnsignedInts $ lines' !! 1

parseRace :: String -> Race
parseRace input = Race (head times) (head distances)
  where
    lines' = lines input
    times = parseUnsignedInts $ filter (/= ' ') $ head lines'
    distances = parseUnsignedInts $ filter (/= ' ') $ lines' !! 1

solutionProducts :: [Race] -> Integer
solutionProducts = product . map nSolutions

nSolutions :: Race -> Integer
nSolutions (Race t d) = floor (mid + diff) - ceiling (mid - diff) + 1 + comp
  where
    mid = fromIntegral t / 2
    diff = sqrt (fromIntegral (t ^ 2 - 4 * d)) / 2
    comp = if isInt $ mid + diff
      then (-2)
      else 0

isInt :: ( RealFrac a) =>a -> Bool
isInt x = floor x == ceiling x

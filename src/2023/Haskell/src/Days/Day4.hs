module Days.Day4 (solve) where
import           AoCUtils.Days  (Solver)
import           AoCUtils.Regex (parseUnsignedInts)
import           Data.List      (intersect)

solve :: Solver
solve input = let
  cards = map parseCard $ lines input
  part1 = solve1 cards
  part2 = solve2 cards
  in (show part1, show part2)

data Card = Card Integer [Integer] [Integer]
  deriving (Show)

parseCard :: String -> Card
parseCard line = Card (head integers) winning have
  where
    integers = parseUnsignedInts line
    (winning, have) = splitAt 10 $ drop 1 integers

solve1 :: [Card] -> Int
solve1 = sum . map points

points :: Card -> Int
points card = let n = nWinning card in if n == 0 then 0 else 2 ^ (n - 1)

nWinning :: Card -> Int
nWinning (Card _ winning have) = length $ intersect winning have

solve2 :: [Card] -> Integer
solve2 = sum . foldr findGenerating []

findGenerating :: Card -> [Integer] -> [Integer]
findGenerating card results = nGenerating : results
  where
    nCopies = nWinning card
    nGenerating = 1 + sum (take nCopies results)

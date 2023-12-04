module Days.Day4 (solve) where
import           AoCUtils.Days  (Solver)
import           AoCUtils.Regex (parseUnsignedInts)
import qualified Data.HashSet   as HS

solve :: Solver
solve input = let
  cards = map parseCard $ lines input
  part1 = solve1 cards
  part2 = solve2 cards
  in (show part1, show part2)

data Card = Card {
  cId      :: Integer,
  cWinning :: [Integer],
  cHave    :: [Integer]
} deriving (Show)

parseCard :: String -> Card
parseCard line = Card {
  cId = head integers,
  cWinning = winning,
  cHave = have}
  where
    integers = parseUnsignedInts line
    (winning, have) = splitAt 10 $ drop 1 integers

solve1 :: [Card] -> Int
solve1 = sum . map points

points :: Card -> Int
points card
  | n == 0 = 0
  | otherwise = 2 ^ (n - 1)
  where
    n = nWinning card

nWinning :: Card -> Int
nWinning (Card {cWinning = winning, cHave = have}) =
  HS.size $ HS.intersection (HS.fromList winning) (HS.fromList have)

solve2 :: [Card] -> Integer
solve2 cards = sum (foldr findGenerating [] cards)

findGenerating :: Card -> [Integer] -> [Integer]
findGenerating card results = nGenerating : results
  where
    nCopies = nWinning card
    nGenerating = 1 + sum (take nCopies results)

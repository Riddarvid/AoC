module Days.Day1 (solve) where
import           AoCUtils.Days (Solver)
import           Data.Char     (isDigit)
import           Data.Foldable (find)
import           Data.List     (tails)
import           Data.Maybe    (mapMaybe)

solve :: Solver
solve input = let
  input' = lines input
  part1 = solve1 input'
  part2 = solve2 input'
  in (show part1, show part2)

solve1 :: [String] -> Integer
solve1 = sum . map (findCalibration . filter isDigit)

solve2 :: [String] -> Integer
solve2 = sum . map (findCalibration . mapMaybe readDigits . tails)

-- Assumes all chars are digits
findCalibration :: String -> Integer
findCalibration line = read [head digits, last digits]
  where
    digits = filter isDigit line

readDigits :: String -> Maybe Char
readDigits [] = Nothing
readDigits line@(c : _)
  | isDigit c = Just c
  | otherwise = parseDigit line

parseDigit :: String -> Maybe Char
parseDigit input = snd <$> find (\(literal, _) -> startsWith literal input) (zip digits ['1' .. '9'])
  where
    digits = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

startsWith :: String -> String -> Bool
startsWith target input = take (length target) input == target

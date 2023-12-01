module Days.Day1 (solve, translateDigits) where
import           AoCUtils.Days (Solver)
import           Data.Char     (isDigit)
import           Data.Foldable (find)
import           Data.Maybe    (fromMaybe)

solve :: Solver
solve input = let
  input' = lines input
  part1 = solve1 input'
  part2 = solve2 input'
  in (show part1, show part2)

solve1 :: [String] -> Integer
solve1 = sum . map findCalibration

solve2 :: [String] -> Integer
solve2 = sum . map (findCalibration . translateDigits)

findCalibration :: String -> Integer
findCalibration line = read [head digits, last digits]
  where
    digits = filter isDigit line

translateDigits :: String -> String
translateDigits [] = []
translateDigits line@(c : cs) = fromMaybe c (parseDigit line) : translateDigits cs

parseDigit :: String -> Maybe Char
parseDigit input = snd <$> find (\(literal, _) -> startsWith literal input) (zip digits ['1' .. '9'])
  where
    digits = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

startsWith :: String -> String -> Bool
startsWith target input = take (length target) input == target

module Days.Day1 (solve) where
import           AoCUtils.Days (Solver)
import           Data.Char     (isDigit)
import           Data.Foldable (find)

solve :: Solver
solve input = let
  input' = lines input
  part1 = solve1 input'
  part2 = solve2 input'
  in (show part1, show part2)

solve1 :: [String] -> Integer
solve1 = sum . map findCalibration

solve2 :: [String] -> Integer
solve2 = sum . map (findCalibration . readDigits)

findCalibration :: String -> Integer
findCalibration line = read [head digits, last digits]
  where
    digits = filter isDigit line

readDigits :: String -> String
readDigits [] = []
readDigits line@(c : cs)
  | isDigit c = c : end
  | otherwise = case parseDigit line of
    Nothing -> end
    Just c' -> c' : end
  where
    end = readDigits cs

parseDigit :: String -> Maybe Char
parseDigit input = snd <$> find (\(literal, _) -> startsWith literal input) (zip digits ['1' .. '9'])
  where
    digits = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

startsWith :: String -> String -> Bool
startsWith target input = take (length target) input == target

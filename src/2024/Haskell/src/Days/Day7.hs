module Days.Day7 (solve) where
import           AoCUtils.Days      (Solver)
import           AoCUtils.Regex     (parseSignedInts)
import           Data.List.NonEmpty (NonEmpty ((:|)))

solve :: Solver
solve input = let
  equations = parseEquations input
  part1 = solve1 equations
  part2 = solve2 equations
  in (show part1, show part2)

data Equation = Equation {
  eTestValue :: Integer,
  _eValues   :: NonEmpty Integer
}

type BinOp a = a -> a -> a

------------ Parsing --------------------

parseEquations :: String -> [Equation]
parseEquations = map parseEquation . lines

parseEquation :: String -> Equation
parseEquation str = case parseSignedInts str of
  (testValue : x : xs) -> Equation testValue (x :| xs)
  _ -> error "Equation needs test value and at least one other value"

---------------------------------------------

solve1 :: [Equation] -> Integer
solve1 = calibrate [(+), (*)]

solve2 :: [Equation] -> Integer
solve2 = calibrate [(+), (*), catInts]

catInts :: (Read a, Show a) => a -> a -> a
catInts a b = read (show a ++ show b)

calibrate :: [BinOp Integer] -> [Equation] -> Integer
calibrate ops = sum . map eTestValue . filter (isPossiblyValid ops)

isPossiblyValid :: [BinOp Integer] -> Equation -> Bool
isPossiblyValid operators (Equation testValue (start :| values)) = go start values
  where
    go acc [] = acc == testValue
    go acc (x : xs)
      | acc > testValue = False
      | otherwise = any (`go` xs) accs
      where
        accs = map (\op -> op acc x) operators

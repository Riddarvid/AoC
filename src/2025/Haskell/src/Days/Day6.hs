{-# LANGUAGE LambdaCase #-}
module Days.Day6 (solve) where
import           AoCUtils.Days  (Solver)
import           AoCUtils.Regex (parseUnsignedInts)
import           Data.List      (transpose)
import           Data.Maybe     (mapMaybe)
import           Utils          (flattenSingletonPartial, split)

data Operator = Add | Mul
  deriving (Show)

data Problem = Problem Operator [Integer]
  deriving (Show)

solve :: Solver
solve input = let
  part1 = solve1 input
  part2 = solve2 input
  in (show part1, show part2)

solve1 :: String -> Integer
solve1 = solveGeneral parseProblemInts1

solve2 :: String -> Integer
solve2 = solveGeneral parseProblemInts2

-- General

solveGeneral :: ([String] -> [[Integer]]) -> String -> Integer
solveGeneral parseIntsFun = sum . map solveProblem . parseProblemsGeneral parseIntsFun

solveProblem :: Problem -> Integer
solveProblem (Problem op values) = foldr1 opFun values
  where
    opFun = case op of
      Mul -> (*)
      Add -> (+)

parseOperators :: String -> [Operator]
parseOperators = mapMaybe parseOperator

parseOperator :: Char -> Maybe Operator
parseOperator = \case
  '*' -> Just Mul
  '+' -> Just Add
  _   -> Nothing

parseProblemsGeneral :: ([String] -> [[Integer]]) -> String -> [Problem]
parseProblemsGeneral parseIntsFun input = zipWith Problem problemOperators problemInts
  where
    inputLines = lines input
    problemInts = parseIntsFun $ init inputLines
    problemOperators = parseOperators $ last inputLines

-- Part 1

parseProblemInts1 :: [String] -> [[Integer]]
parseProblemInts1 = transpose . map parseUnsignedInts

-- Part 2

parseProblemInts2 :: [String] -> [[Integer]]
parseProblemInts2 input = intsByProblem
  where
    singletonIntsWithSpaces = map parseUnsignedInts $ transpose input
    singletonIntsByProblem = split [] singletonIntsWithSpaces
    intsByProblem = map (map flattenSingletonPartial) singletonIntsByProblem

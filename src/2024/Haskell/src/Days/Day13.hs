module Days.Day13 (solve) where
import           AoCUtils.Days   (Solver)
import           AoCUtils.Regex  (parseUnsignedInts)
import           Data.List       (sortBy, sortOn, transpose)
import           Data.List.Split (chunksOf)
import           Data.Maybe      (catMaybes, mapMaybe)

solve :: Solver
solve input = let
  machines = parseMachines input
  part1 = solve1 machines
  in (show part1, "")

data Machine = Machine Equation Equation
data Equation = Equation Integer Integer Integer

parseMachines :: String -> [Machine]
parseMachines = map parseMachine . chunksOf 4 . lines

parseMachine :: [String] -> Machine
parseMachine strings = case strings of
  (str1 : str2 : str3 : _) -> case equations of
    [equation1, equation2] -> Machine equation1 equation2
    _                      -> error "Wrong number of equations"
    where
      equations = map parseEquation $ transpose [nums1, nums2, nums3]
      nums1 = parseUnsignedInts str1
      nums2 = parseUnsignedInts str2
      nums3 = parseUnsignedInts str3
  _                        -> error "Wrong number of strings"

parseEquation :: [Integer] -> Equation
parseEquation [x, y, z] = Equation x y z
parseEquation _         = error "Wrong number of parameters"

solve1 :: [Machine] -> Integer
solve1 = sum . mapMaybe bestSolutionNaive

bestSolutionNaive :: Machine -> Maybe Integer
bestSolutionNaive machine = case sortOn tokenCost solutions of
  []         -> Nothing
  (best : _) -> Just $ tokenCost best
  where
    solutions = filter (isMachineSolution machine) [(nA, nB) | nA <- [0 .. 100], nB <- [0 .. 100]]

isMachineSolution :: Machine -> (Integer, Integer) -> Bool
isMachineSolution (Machine equ1 equ2) (nA, nB) = isEquationSolution equ1 nA nB && isEquationSolution equ2 nA nB

isEquationSolution :: Equation -> Integer -> Integer -> Bool
isEquationSolution (Equation x y z) nA nB = nA * x + nB * y == z

tokenCost :: (Integer, Integer) -> Integer
tokenCost (nA, nB) = nA * 3 + nB

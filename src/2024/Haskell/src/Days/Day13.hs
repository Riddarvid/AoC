module Days.Day13 (solve) where
import           AoCUtils.Days   (Solver)
import           AoCUtils.Regex  (parseUnsignedInts)
import           Data.List       (transpose)
import           Data.List.Split (chunksOf)
import           Data.Maybe      (mapMaybe)
import           Data.Ratio      (denominator, numerator, (%))

solve :: Solver
solve input = let
  machines = parseMachines input
  part1 = solve1 machines
  part2 = solve2 machines
  in (show part1, show part2)

data Machine = Machine Equation Equation
data Equation = Equation Integer Integer Integer
  deriving (Show)
data MachineSolution = SingleSolution Rational Rational | InfSolution Equation

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
solve1 = findMinTokens

solve2 :: [Machine] -> Integer
solve2 = findMinTokens . map (increaseMachineTarget 10000000000000)

increaseMachineTarget :: Integer -> Machine -> Machine
increaseMachineTarget n (Machine equ1 equ2) =
  Machine (increaseEquationTarget n equ1) (increaseEquationTarget n equ2)

increaseEquationTarget :: Integer -> Equation -> Equation
increaseEquationTarget n (Equation x y z) = Equation x y (z + n)

findMinTokens :: [Machine] -> Integer
findMinTokens = sum . mapMaybe (bestSolution . findSolutions)

findSolutions :: Machine -> MachineSolution
findSolutions (Machine equ1@(Equation x1 y1 z1) (Equation x2 y2 z2))
  | y == 0 = InfSolution equ1
  | otherwise = SingleSolution nX nY
  where
    factor = x1 % x2
    y = fromInteger y1 - factor * fromInteger y2
    z = fromInteger z1 - factor * fromInteger z2
    nY = z / y
    nX = (fromInteger z1 - nY * fromInteger y1) / fromInteger x1

bestSolution :: MachineSolution -> Maybe Integer
bestSolution solution = case solution of
  SingleSolution nX nY -> case (denominator nX, denominator nY) of
    (1, 1) -> Just $ 3 * numerator nX + numerator nY
    _      -> Nothing
  InfSolution _equ -> undefined

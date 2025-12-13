module Solvers (solvers) where
import           AoCUtils.Days (Solver)
import qualified Days.Day1     as Day1
import qualified Days.Day2     as Day2
import qualified Days.Day3     as Day3
import qualified Days.Day4     as Day4
import qualified Days.Day5     as Day5
import qualified Days.Day6     as Day6
import qualified Days.Day7     as Day7
import qualified Days.Day8     as Day8

solvers :: [Solver]
solvers = [
  Day1.solve, Day2.solve, Day3.solve, Day4.solve, Day5.solve, Day6.solve,
  Day7.solve, Day8.solve]

module Solvers (solvers) where
import           AoCUtils.Days (Solver)
import qualified Days.Day1     as Day1
import qualified Days.Day2     as Day2
import qualified Days.Day3     as Day3
import qualified Days.Day4     as Day4

solvers :: [Solver]
solvers = [Day1.solve, Day2.solve, Day3.solve, Day4.solve]

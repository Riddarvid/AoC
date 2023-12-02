module Utils.Days (solvers) where

import           AoCUtils.Days (Solver)
import           Days.Day1     as Day1 (solve)
import           Days.Day2     as Day2 (solve)

solvers :: [Solver]
solvers = [Day1.solve, Day2.solve]

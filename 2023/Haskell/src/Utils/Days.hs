module Utils.Days (solvers) where

import           AoCUtils.Days (Solver)
import           Days.Day1     as Day1 (solve)

solvers :: [Solver]
solvers = [Day1.solve]

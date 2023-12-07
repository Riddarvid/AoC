module Utils.Days (solvers) where

import           AoCUtils.Days (Solver)
import           Days.Day1     as Day1 (solve)
import           Days.Day2     as Day2 (solve)
import           Days.Day3     as Day3 (solve)
import           Days.Day4     as Day4 (solve)
import           Days.Day5     as Day5 (solve)
import           Days.Day6     as Day6 (solve)
import           Days.Day7     as Day7 (solve)

solvers :: [Solver]
solvers = [Day1.solve, Day2.solve, Day3.solve, Day4.solve, Day5.solve, Day6.solve, Day7.solve]

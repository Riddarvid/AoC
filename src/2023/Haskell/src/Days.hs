module Days (solvers) where

import           AoCUtils.Days (Solver)
import           Days.Day01    as Day01 (solve)
import           Days.Day02    as Day02 (solve)
import           Days.Day03    as Day03 (solve)
import           Days.Day04    as Day04 (solve)
import           Days.Day05    as Day05 (solve)
import           Days.Day06    as Day06 (solve)
import           Days.Day07    as Day07 (solve)
import           Days.Day08    as Day08 (solve)
import           Days.Day09    as Day09 (solve)
import           Days.Day10    as Day10 (solve)
import           Days.Day11    as Day11 (solve)
import           Days.Day12    as Day12 (solve)
import           Days.Day13    as Day13 (solve)
import           Days.Day14    as Day14 (solve)
import           Days.Day15    as Day15 (solve)
import           Days.Day16    as Day16 (solve)
import           Days.Day17    as Day17 (solve)
import           Days.Day18    as Day18 (solve)
import           Days.Day19    as Day19 (solve)
import           Days.Day20    as Day20 (solve)

solvers :: [Solver]
solvers =
  [
    Day01.solve, Day02.solve, Day03.solve, Day04.solve, Day05.solve, Day06.solve, Day07.solve,
    Day08.solve, Day09.solve, Day10.solve, Day11.solve, Day12.solve, Day13.solve, Day14.solve,
    Day15.solve, Day16.solve, Day17.solve, Day18.solve, Day19.solve, Day20.solve
  ]

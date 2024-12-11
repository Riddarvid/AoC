module Solvers (solvers) where
import           AoCUtils.Days (Solver)
import qualified Days.Day1     as D1
import qualified Days.Day10    as D10
import qualified Days.Day2     as D2
import qualified Days.Day3     as D3
import qualified Days.Day4     as D4
import qualified Days.Day5     as D5
import qualified Days.Day6     as D6
import qualified Days.Day7     as D7
import qualified Days.Day8     as D8
import qualified Days.Day9     as D9

solvers :: [Solver]
solvers =
  [
    D1.solve, D2.solve, D3.solve, D4.solve, D5.solve, D6.solve, D7.solve, D8.solve,
    D9.solve, D10.solve
  ]

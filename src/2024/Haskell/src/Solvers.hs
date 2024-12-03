module Solvers (solvers) where
import           AoCUtils.Days (Solver)
import qualified Days.Day1     as D1
import qualified Days.Day2     as D2
import qualified Days.Day3     as D3

solvers :: [Solver]
solvers = [D1.solve, D2.solve, D3.solve]

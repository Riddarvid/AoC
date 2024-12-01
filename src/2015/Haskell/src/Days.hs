module Days (solvers) where
import qualified Days.Day21 as Day21
import AoCUtils.Days (Solver)

solvers :: [Solver]
solvers = replicate 20 undefined ++ [Day21.solve]
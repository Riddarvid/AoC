module Main (main) where

import           Control.Monad.State (runState)
import           Day3                (solve)
import           System.Random       (mkStdGen)

main :: IO ()
main = do
  input <- readFile "app/input/input3.txt"
  let ((part1, part2), _) = runState Day3.solve (mkStdGen 10)
  putStrLn "Part1:"
  print part1
  putStrLn ""
  putStrLn "Part2:"
  print part2

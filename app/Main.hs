module Main (main) where

import           Day2 (solve)

main :: IO ()
main = do
  input <- readFile "app/input/input2.txt"
  let (part1, part2) = Day2.solve input
  putStrLn "Part1:"
  print part1
  putStrLn ""
  putStrLn "Part2:"
  print part2

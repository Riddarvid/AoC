module Day1 (solve) where

import           Input (stringToInts)

solve :: String -> (Integer, Integer)
solve input = (part1, part2)
  where
    measurements = stringToInts input
    part1 = nIncs measurements 1
    part2 = nIncs measurements 3


-- Takes two parameters, the list of measurements and th size of the sliding window.
nIncs :: [Integer] -> Int -> Integer
nIncs _ n
  | n <= 0 = error "Window size must be positive"
nIncs [] _ = 0
nIncs xs n
  | length xs <= n = 0
nIncs xs@(x:rest) n
  | xs !! n > x = 1 + nIncs rest n
  | otherwise = 0 + nIncs rest n

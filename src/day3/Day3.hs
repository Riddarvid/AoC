module Day3 (solve) where

import           Control.Monad.State.Lazy
import           System.Random

solve :: State StdGen (Integer, Integer)
solve = do
  n1 <- generateRandom
  n2 <- generateRandom
  return (n1, n2)

generateRandom :: State StdGen Integer
generateRandom = do
  gen <- get
  let (n, gen') = randomR (1, 100) gen
  put gen'
  return n

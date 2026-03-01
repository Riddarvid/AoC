module Main (main) where

import           AoCUtils.Config      (mkConfig)
import           AoCUtils.Days        (readInput)
import           AoCUtils.Interactive (aocMain)
import           Days.Day9.Graphics   as G9
import qualified Days.Day9.Graphics   as G9
import           Solvers              (solvers)

inputPath :: FilePath
inputPath = "../../../data/inputs/2025"

resultsPath :: FilePath
resultsPath = "../../../data/results/2025"

main :: IO ()
main = aocMain $ mkConfig solvers inputPath resultsPath visualize

visualize :: String -> IO ()
visualize "9" = vis9
visualize _   = putStrLn "No such visualization"

vis9 :: IO ()
vis9 = readInput inputPath 9 >>= G9.drawTiles

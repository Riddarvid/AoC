module Main (main) where

import           AoCUtils.Config      (mkConfig)
import           AoCUtils.Interactive (aocMain)
import           Solvers              (solvers)

inputPath :: FilePath
inputPath = "../../../data/inputs/2025"

resultsPath :: FilePath
resultsPath = "../../../data/results/2025"

main :: IO ()
main = aocMain $ mkConfig solvers inputPath resultsPath

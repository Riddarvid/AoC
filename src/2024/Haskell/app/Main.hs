module Main (main) where

import           AoCUtils.Config      (mkConfig)
import           AoCUtils.Interactive (aocMain)
import           Solvers              (solvers)

inputPath :: FilePath
inputPath = "../../../data/inputs/2024"


resultsPath :: FilePath
resultsPath = "../../../data/results/2023"

main :: IO ()
main = aocMain $ mkConfig solvers inputPath resultsPath

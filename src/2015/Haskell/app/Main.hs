module Main (main) where
import           AoCUtils.Config      (mkConfig)
import           AoCUtils.Interactive (aocMain)
import           Days                 (solvers)

inputPath :: FilePath
inputPath = "../../../data/inputs/2023"

main :: IO ()
main = aocMain cfg
  where
    cfg = mkConfig solvers inputPath "../../../data/results/2023"
import           AoCUtils.Config (mkConfig)
import           AoCUtils.Test   (aocTests)
import           Solvers         (solvers)

inputPath :: FilePath
inputPath = "../../../data/inputs/2025"

resultsPath :: FilePath
resultsPath = "../../../data/results/2025"

main :: IO ()
main = aocTests $ mkConfig solvers inputPath resultsPath

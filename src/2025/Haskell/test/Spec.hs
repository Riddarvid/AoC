import           AoCUtils.Config (mkConfig)
import           AoCUtils.Test   (aocTests)
import           Solvers         (solvers)

inputPath :: FilePath
inputPath = "../../../data/inputs/2024"

resultsPath :: FilePath
resultsPath = "../../../data/results/2024"

main :: IO ()
main = aocTests $ mkConfig solvers inputPath resultsPath

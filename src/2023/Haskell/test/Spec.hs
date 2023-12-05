import           AoCUtils.Config (mkConfig)
import           AoCUtils.Test   (aocTests)
import           Utils.Days      (solvers)
main :: IO ()
main = aocTests cfg
  where
    cfg = mkConfig solvers "../../../data/inputs/2023" "../../../data/results/2023" Nothing

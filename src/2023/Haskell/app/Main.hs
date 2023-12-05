module Main (main) where
import           AoCUtils.Config      (mkConfig)
import           AoCUtils.Interactive (aocMain)
import           Utils.Days           (solvers)

main :: IO ()
main = aocMain cfg
  where
    cfg = mkConfig solvers "../../../data/inputs/2023" "../../../data/results/2023" Nothing

module Main (main) where
import           AoCUtils.Interactive (aocMain)
import AoCUtils.Config (mkConfig)
import           Utils.Days           (solvers)

main :: IO ()
main = aocMain cfg
  where
    cfg = mkConfig solvers "../../../data/inputs/2023" "../../../data/results/2023" Nothing
module Main (main) where
import           AoCUtils.Config      (mkConfig)
import           AoCUtils.Days        (readInput)
import           AoCUtils.Interactive (aocMain)
import           Days                 (solvers)
import           Days.Day10.Graphics  (renderDay10Loop, renderDay10LoopScaled)

inputPath :: FilePath
inputPath = "../../../data/inputs/2023"

main :: IO ()
main = aocMain cfg
  where
    cfg = mkConfig solvers inputPath "../../../data/results/2023" (Just visualizations)

visualizations :: String -> IO ()
visualizations "10l"  = vis11Loop
visualizations "10ls" = vis11LoopScaled
visualizations _      = error "No matching visualization"

vis11Loop :: IO ()
vis11Loop = do
  input <- readInput inputPath 10
  renderDay10Loop input

vis11LoopScaled :: IO ()
vis11LoopScaled = do
  input <- readInput inputPath 10
  renderDay10LoopScaled input

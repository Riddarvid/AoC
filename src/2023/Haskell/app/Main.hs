module Main (main) where
import           AoCUtils.Config      (mkConfig)
import           AoCUtils.Days        (readInput)
import           AoCUtils.Interactive (aocMain)
import           Days                 (solvers)
import           Days.Day10.Graphics  (renderDay10Loop, renderDay10LoopScaled)
import           Days.Day14.Graphics  (animateDay14)

inputPath :: FilePath
inputPath = "../../../data/inputs/2023"

main :: IO ()
main = aocMain cfg
  where
    cfg = mkConfig solvers inputPath "../../../data/results/2023" (Just visualizations)

visualizations :: String -> IO ()
visualizations "10l"  = vis10Loop
visualizations "10ls" = vis10LoopScaled
visualizations "14"   = vis14
visualizations _      = error "No matching visualization"

vis10Loop :: IO ()
vis10Loop = do
  input <- readInput inputPath 10
  renderDay10Loop input

vis10LoopScaled :: IO ()
vis10LoopScaled = do
  input <- readInput inputPath 10
  renderDay10LoopScaled input

vis14 :: IO ()
vis14 = do
  input <- readInput inputPath 14
  animateDay14 input

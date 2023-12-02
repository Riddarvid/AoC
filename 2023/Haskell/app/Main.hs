module Main (main) where
import           AoCUtils.Interactive (aocMain)
import           Utils.Days           (solvers)

main :: IO ()
main = aocMain solvers visualizations

visualizations :: String -> IO ()
visualizations visId = error $ "No visualization implemented for " ++ visId
{- HLINT ignore "Use unwords" -}
module Days.Day10.GLPK (
  createLpString,
  solveLp,
  getLpSolution
) where
import           Control.Monad     (void)
import           Data.Char         (isDigit)
import qualified Data.HashMap.Lazy as HM
import           Data.List         (intercalate)
import           Days.Day10.Types  (Button,
                                    Machine (mButtonSchematics, mJoltageReqs))
import           System.Process    (CreateProcess (std_out),
                                    StdStream (NoStream), createProcess, proc,
                                    waitForProcess)

-- Defining LPs

createLpString :: Machine -> String
createLpString m =
  "Minimize\n" ++
  sumObjectiveString ++ "\n" ++
  "Subject To\n" ++
  constraintsString ++ "\n" ++
  "Bounds\n" ++
  boundsString ++ "\n" ++
  "Generals\n" ++
  generalsString ++ "\n" ++
  "End"
  where
    sumObjectiveString = mkSumObjective m
    constraintsString = mkConstraints m
    boundsString = mkBounds m
    generalsString = mkGenerals m

mkSumObjective :: Machine -> String
mkSumObjective m = " obj: " ++ intercalate " + " varsStrings ++ "\n"
  where
    varsStrings = mkVarStrings m

mkConstraints :: Machine -> String
mkConstraints m = unlines constraintStrings
  where
    constraintStrings = map (mkConstraint m) [0 .. nConstraints m - 1]

mkConstraint :: Machine -> Int -> String
mkConstraint m index =
  " c" ++ show index ++ ": " ++ intercalate " + " varStrings ++ " = " ++ show result
  where
    result = mJoltageReqs m HM.! index
    vars = map fst $
      filter (\(_, button) -> contributesToJoltage index button) $
      zip [0 ..] $
      mButtonSchematics m
    varStrings = map mkVarString vars

contributesToJoltage :: Int -> Button -> Bool
contributesToJoltage = elem

mkBounds :: Machine -> String
mkBounds m = unlines boundsStrings
  where
    varsStrings = mkVarStrings m
    boundsStrings = map (\xi -> " " ++ xi ++ " >= 0") varsStrings

mkGenerals :: Machine -> String
mkGenerals m = " " ++ intercalate " " varsStrings ++ "\n"
  where
    varsStrings = mkVarStrings m

mkVarStrings :: Machine -> [String]
mkVarStrings = map mkVarString . varIndexes

mkVarString :: Int -> String
mkVarString i = "x" ++ show i

varIndexes :: Machine -> [Int]
varIndexes m = [0 .. nVars m - 1]

nVars :: Machine -> Int
nVars = length . mButtonSchematics

nConstraints :: Machine -> Int
nConstraints = HM.size . mJoltageReqs

-- Solving LPs

solveLp :: FilePath -> FilePath -> IO ()
solveLp inputPath outputPath = do
  let process = (proc "glpsol" ["--lp", inputPath, "-w", outputPath]){std_out = NoStream}
  (_, _, _, processHandle) <- createProcess process
  void $ waitForProcess processHandle

-- Fetching results from LPs

getLpSolution :: FilePath -> IO Int
getLpSolution solutionPath = do
  solution <- readFile solutionPath
  return $ parseResult solution

parseResult :: String -> Int
parseResult solution = read resultString
  where
    solutionLine = lines solution !! 6
    resultString = takeWhile isDigit $ drop 2 $ dropWhile (/= '=') solutionLine

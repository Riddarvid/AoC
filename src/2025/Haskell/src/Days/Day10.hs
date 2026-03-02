{-# LANGUAGE LambdaCase #-}
{- HLINT ignore "Use unwords" -}
module Days.Day10 (
  solve,
  computeLinearProblems
) where
import           AoCUtils.Days                (Solver)
import           Control.Applicative          ((<|>))
import           Data.Char                    (isDigit)
import           Data.HashMap.Lazy            (HashMap)
import qualified Data.HashMap.Lazy            as HM
import           Days.Day10.GLPK              (createLpString, getLpSolution,
                                               solveLp)
import           Days.Day10.Types             (Button, Indicators, Joltages,
                                               Machine (Machine, mButtonSchematics, mJoltageReqs, mTargetIndicators))
import           System.FilePath              ((</>))
import           Text.ParserCombinators.ReadP (ReadP, between, char, many,
                                               many1, readP_to_S, satisfy,
                                               sepBy1, skipSpaces)

-- Looking at the data, it looks like we have at most 13 buttons per machine.
-- This yields 2^13 = 8192 permutations per machine,
-- which should be feasible to iterate through.

mkEmptyIndicators :: Indicators -> Indicators
mkEmptyIndicators = HM.map (const False)

solve :: Solver
solve input = let
  machines = map parseMachine $ lines input
  part1 = solve1 machines
  in (show part1, "Defining the LP problems for part two can be done by invoking computeLinearProblems")

-- Parsing

parseMachine :: String -> Machine
parseMachine mString = case readP_to_S machineParser mString of
  [(m, "")] -> m
  [(_, _)]  -> error "String not fully parsed"
  []        -> error "No parse"
  _         -> error "Multiple potential parses"

machineParser :: ReadP Machine
machineParser = do
  indicators <- indicatorsParser
  skipSpaces
  buttons <- buttonsParser
  skipSpaces
  joltage <- joltageParser
  return $ Machine {mTargetIndicators=indicators, mButtonSchematics=buttons, mJoltageReqs=joltage}

indicatorsParser :: ReadP Indicators
indicatorsParser = do
  indicatorsString <- between (char '[') (char ']') $ many indicatorParser
  return $ listToHashMap $ map charToIndicator indicatorsString

indicatorParser :: ReadP Char
indicatorParser = char '#' <|> char '.'

charToIndicator :: Char -> Bool
charToIndicator = \case
  '#' -> True
  '.' -> False
  _ -> error "Not an indicator char"

buttonsParser :: ReadP [Button]
buttonsParser = sepBy1 buttonParser skipSpaces

buttonParser :: ReadP Button
buttonParser = between (char '(') (char ')') $
  sepBy1 intParser (char ',')

intParser :: ReadP Int
intParser = do
  digitString <- many1 (satisfy isDigit)
  return $ read digitString

joltageParser :: ReadP Joltages
joltageParser = do
  joltageList <- between (char '{') (char '}') $ sepBy1 intParser (char ',')
  return $ listToHashMap joltageList

-- Solutions

solve1 :: [Machine] -> Int
solve1 = sum . map findFewestPresses1

-- Dynamic programming
-- Case one: button is not pressed. Do not increase number pressed.
-- Case two: button is pressed. Increment number pressed.
-- Potential pruning: keep record of best solution found so far.
-- Only continue if current solution has not gone over that.
findFewestPresses1 :: Machine -> Int
findFewestPresses1 m =
  case go (mkEmptyIndicators targetIndicators) (mButtonSchematics m) of
    Nothing -> error "No solution found"
    Just n  -> n
  where
    targetIndicators = mTargetIndicators m
    go indicators buttons
      | indicators == targetIndicators = Just 0
      | otherwise = case buttons of
        []                  -> Nothing
        (button : buttons') -> do
          let n1 = go indicators buttons' -- case one
          let n2 = (+1) <$> go (pressButton indicators button) buttons' -- case two
          minSolution n1 n2

pressButton :: Indicators -> Button -> Indicators
pressButton = foldr flipIndicator

flipIndicator :: Int -> Indicators -> Indicators
flipIndicator = HM.adjust not

minSolution :: Ord a => Maybe a -> Maybe a -> Maybe a
minSolution s1 s2 = case s1 of
  Nothing -> s2
  Just res1 -> case s2 of
    Nothing   -> Just res1
    Just res2 -> Just (min res1 res2)

-- Part 2

computeLinearProblems :: FilePath -> String -> IO ()
computeLinearProblems outputDir input = do
  let machines = map parseMachine $ lines input
  let nMachines = length machines
  defineAndWriteLps outputDir machines
  solveLps nMachines outputDir
  results <- getLpResults nMachines outputDir
  let result = sum results
  putStrLn $ "Result: " ++ show result

-- Define LPs

defineAndWriteLps :: FilePath -> [Machine] -> IO ()
defineAndWriteLps resourceDir machines = do
  let linearProblemsStrings = map createLpString machines
  mapM_ (uncurry $ writeLp resourceDir) $ zip [0 ..] linearProblemsStrings

writeLp :: FilePath -> Int -> String -> IO ()
writeLp resourceDir index = writeFile fileName
  where
    fileName = mkProblemPath resourceDir index

-- Solve LPs

solveLps :: Int -> FilePath -> IO ()
solveLps nMachines resourceDir = mapM_ (uncurry solveLp) fileNames
  where
    fileNames =
      map (\n -> (mkProblemPath resourceDir n, mkSolutionPath resourceDir n))
        [0 .. nMachines - 1]

-- Fetch results from LPs

getLpResults :: Int -> FilePath -> IO [Int]
getLpResults nMachines resourceDir = mapM getLpSolution fileNames
  where
    fileNames = map (mkSolutionPath resourceDir) [0 .. nMachines - 1]

-- General

-- Maps list index to list item
listToHashMap :: [a] -> HashMap Int a
listToHashMap = HM.fromList . zip [0 ..]

mkProblemPath :: FilePath -> Int -> FilePath
mkProblemPath dir n = dir </> "problems" </> ("problem" ++ show n ++ ".lp")

mkSolutionPath :: FilePath -> Int -> FilePath
mkSolutionPath dir n = dir </> "solutions" </> ("solution" ++ show n ++ ".sol")

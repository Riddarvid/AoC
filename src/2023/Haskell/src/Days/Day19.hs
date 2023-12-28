{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Days.Day19 (solve) where
import           AoCUtils.Days        (Solver)
import           AoCUtils.Regex       (parseSignedInts)
import           Control.Monad.Reader (MonadReader (ask), Reader, runReader)
import           Data.HashMap.Lazy    (HashMap)
import qualified Data.HashMap.Lazy    as HM
import           Data.Maybe           (fromJust)
import           Prelude              hiding (GT, LT)
import           Text.Parsec          (Parsec, between, char, digit, letter,
                                       many1, oneOf, optionMaybe, optional,
                                       parse, sepBy1, try, (<|>))

-- Parts

data Part = Part {
  px :: Int,
  pm :: Int,
  pa :: Int,
  ps :: Int
}

xmasScore :: Part -> Int
xmasScore (Part x m a s) = x + m + a + s

-- Workflows

newtype Workflow = WF [Rule]

type Workflows = HashMap String Workflow

data Rule = Rule (Maybe Cond) Result

data Cond = Cond PartType Op Int

data PartType = X | M | A | S

data Op = LT | GT

data Result = ResString String | ResAcc | ResRej

-- Parser

type Parser = Parsec String ()

-- Main solver

solve :: Solver
solve input = let
  (workflows, parts) = parseInput input
  part1 = solve1 workflows parts
  part2 = solve2 workflows
  in (show part1, show part2)

-- Parsing

parseInput :: String -> (HashMap String Workflow, [Part])
parseInput input = (HM.fromList $ map parseWorkflow wfStrings, map parsePart pStrings)
  where
    (wfStrings, pStrings') = span (/= "") $ lines input
    pStrings = tail pStrings'

parsePart :: String -> Part
parsePart str = case parseSignedInts str of
  [x, m, a, s] -> Part x m a s
  _            -> error "Invalid part"

parseWorkflow :: String -> (String, Workflow)
parseWorkflow str = case parse fullWorkflowParser "" str of
  Left err  -> error $ "Parse error: " ++ show err
  Right res -> res

fullWorkflowParser :: Parser (String, Workflow)
fullWorkflowParser = do
  name <- many1 letter
  workflow <- between (char '{') (char '}') workflowParser
  return (name, workflow)

workflowParser :: Parser Workflow
workflowParser = do
  rules <- sepBy1 ruleParser (char ',')
  return $ WF rules

ruleParser :: Parser Rule
ruleParser = do
  cond <- optionMaybe $ try parseCond
  optional $ char ':'
  res <- parseResult
  return $ Rule cond res

parseCond :: Parser Cond
parseCond = do
  prop <- oneOf "xmas"
  op <- oneOf "<>"
  val <- read <$> many1 digit
  return $ makeCond prop op val
  where
    makeCond :: Char -> Char -> Int -> Cond
    makeCond prop op = Cond prop' op'
      where
        prop' = case prop of
          'x' -> X
          'm' -> M
          'a' -> A
          's' -> S
          _   -> error "Invalid variable"
        op' = case op of
          '<' -> LT
          '>' -> GT
          _   -> error "Invalid operator"

parseResult :: Parser Result
parseResult =
  try (ResAcc <$ char 'A')
  <|> try (ResRej <$ char 'R')
  <|> (ResString <$> many1 letter)

-- Part 1

solve1 :: HashMap String Workflow -> [Part] -> Int
solve1 workflows = sum . map xmasScore . filter (accepts workflows inWorkflow)
  where
    inWorkflow = fromJust $ HM.lookup "in" workflows

accepts :: HashMap String Workflow -> Workflow -> Part -> Bool
accepts workflows (WF rules) = accepts' workflows rules

accepts' :: HashMap String Workflow -> [Rule] -> Part -> Bool
accepts' _ [] _ = error "Final rule should always be applicable"
accepts' workflows (Rule maybeCond res : rules) part = if applicable
    then case res of
      ResString next -> accepts workflows (fromJust $ HM.lookup next workflows) part
      ResAcc -> True
      ResRej -> False
    else accepts' workflows rules part
  where
    applicable = case maybeCond of
      Nothing   -> True
      Just cond -> satisfies cond part

satisfies :: Cond -> Part -> Bool
satisfies (Cond partType op val) part = partF part `opF` val
  where
    partF = case partType of
      X -> px
      M -> pm
      A -> pa
      S -> ps
    opF = case op of
      LT -> (<)
      GT -> (>)

-- Part 2

data Limit = Limit {
  lx :: (Int, Int),
  lm :: (Int, Int),
  la :: (Int, Int),
  ls :: (Int, Int)
} deriving (Show)

mkBaseLimit :: Limit
mkBaseLimit = Limit range range range range
  where
    range = (1, 4000)

newtype LimitMonad a = LM (Reader Workflows a)
  deriving (Functor, Applicative, Monad, MonadReader Workflows)

runLimitMonad :: LimitMonad a -> Workflows -> a
runLimitMonad (LM m) = runReader m

solve2 :: HashMap String Workflow -> Integer
solve2 workflows = runLimitMonad (nAccepting inWorkflow mkBaseLimit) workflows
  where
    inWorkflow = fromJust $ HM.lookup "in" workflows

-- Idea: Should we exit early if a limit is impossible? We will handle this case via
-- freeCombinations, but could be more efficient this way.
nAccepting :: Workflow -> Limit -> LimitMonad Integer
nAccepting (WF rules) = nAccepting' rules

-- Todo: rewrite using some fold or traverse or smth
nAccepting' :: [Rule] -> Limit -> LimitMonad Integer
nAccepting' [] _ = error "Should not be able to happen"
nAccepting' (Rule mCond res : rules) limit = case mCond of
  Nothing -> nAcceptingRes res limit
  Just cond -> do
    let (posLim, negLim) = updateLimit limit cond
    posAcc <- nAcceptingRes res posLim
    negAcc <- nAccepting' rules negLim
    return (posAcc + negAcc)

nAcceptingRes :: Result -> Limit -> LimitMonad Integer
nAcceptingRes res limit = case res of
  ResRej -> return 0
  ResAcc -> return $ freeCombinations limit
  ResString next -> do
    wfs <- ask
    let wf = fromJust $ HM.lookup next wfs
    nAccepting wf limit

freeCombinations :: Limit -> Integer
freeCombinations (Limit x m a s) = product $ map (toInteger . free) [x, m, a, s]
  where
    free :: (Int, Int) -> Int
    free (low, high)
      | high < low = 0
      | otherwise = high - low + 1

-- Given a limit and a condition, returns the limits imposed by the condition,
-- positive and negative.
updateLimit :: Limit -> Cond -> (Limit, Limit)
updateLimit limit (Cond partType op val) = (posLim', negLim')
  where
    lf = case partType of
      X -> lx
      M -> lm
      A -> la
      S -> ls
    (currentLow, currentHigh) = lf limit
    posLim = case op of
      LT -> (currentLow, min currentHigh (val - 1))
      GT -> (max currentLow (val + 1), currentHigh)
    posLim' = updateLimit' limit partType posLim
    negLim = case op of
      LT -> (max currentLow val, currentHigh)
      GT -> (currentLow, min currentHigh val)
    negLim' = updateLimit' limit partType negLim

updateLimit' :: Limit -> PartType -> (Int, Int) -> Limit
updateLimit' limit partType newLim = case partType of
  X -> limit{lx = newLim}
  M -> limit{lm = newLim}
  A -> limit{la = newLim}
  S -> limit{ls = newLim}

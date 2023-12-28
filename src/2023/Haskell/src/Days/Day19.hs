{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Days.Day19 (solve) where
import           AoCUtils.Days     (Solver)
import           AoCUtils.Regex    (parseSignedInts)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import           Data.Maybe        (fromJust)
import           Debug.Trace       (trace)
import           Prelude           hiding (GT, LT)
import           Text.Parsec       (Parsec, between, char, digit, letter, many1,
                                    oneOf, option, optionMaybe, optional, parse,
                                    sepBy1, try, (<|>))

data Part = Part {
  px :: Int,
  pm :: Int,
  pa :: Int,
  ps :: Int
}

data PartType = X | M | A | S

xmasScore :: Part -> Int
xmasScore (Part x m a s) = x + m + a + s

newtype Workflow = WF [Rule]

data Rule = Rule (Maybe Cond) Result

data Cond = Cond PartType Op Int

data Op = LT | GT

data Result = ResString String | ResAcc | ResRej

type Parser = Parsec String ()

solve :: Solver
solve input = let
  (workflows, parts) = parseInput input
  part1 = solve1 workflows parts
  in (show part1, "")

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

nAccepting :: HashMap String Workflow -> Workflow -> Integer
nAccepting workflows (WF rules) = nAccepting' workflows rules

nAccepting' :: HashMap String Workflow -> [Rule] -> Integer
nAccepting' workflows rules = undefined

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

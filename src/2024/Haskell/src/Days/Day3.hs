module Days.Day3 (solve) where
import           AoCUtils.Days        (Solver)
import           Control.Monad        (void)
import           Data.Char            (isDigit)
import           Data.Void            (Void)
import           Text.Megaparsec      (MonadParsec (eof, parseError, takeWhile1P, try),
                                       Parsec, anySingle, parse, (<|>))
import           Text.Megaparsec.Byte (string)
import           Text.Megaparsec.Char (char)

solve :: Solver
solve input = let
  instrs = parseInstructions input
  part1 = solve1 instrs
  part2 = solve2 instrs
  in (show part1, show part2)

solve1 :: [Instruction] -> Int
solve1 = sum . map executeMul . filterMuls

solve2 :: [Instruction] -> Int
solve2 = sum . map executeMul . filterActiveMuls

data Instruction = IMul Int Int | IDo | IDont

isDo :: Instruction -> Bool
isDo IDo = True
isDo _   = False

data Mul = Mul Int Int

filterMuls :: [Instruction] -> [Mul]
filterMuls [] = []
filterMuls (x : xs) = case x of
  IMul a b -> Mul a b : rest
  _        -> rest
  where
    rest = filterMuls xs

filterActiveMuls :: [Instruction] -> [Mul]
filterActiveMuls [] = []
filterActiveMuls (x : xs) = case x of
  IDont -> let
    xs' = dropWhile (not . isDo) xs
    in filterActiveMuls xs'
  IDo -> rest
  IMul a b -> Mul a b : rest
  where
    rest = filterActiveMuls xs

executeMul :: Mul -> Int
executeMul (Mul a b) = a * b

------------ Parsing ----------------------

parseInstructions :: String -> [Instruction]
parseInstructions input = case parse instructionsParser "" input of
  Right instrs -> instrs
  Left _       -> error "Parsing error"

instructionsParser :: Parsec Void String [Instruction]
instructionsParser = eofParser <|> instrParser' <|> skipParser
  where
    eofParser = try eof >> return []
    instrParser' = do
      instr <- try instructionParser
      instrs <- instructionsParser
      return (instr : instrs)
    skipParser = void anySingle >> instructionsParser

instructionParser :: Parsec Void String Instruction
instructionParser = try mulParser <|> try doParser <|> try dontParser

mulParser :: Parsec Void String Instruction
mulParser = do
  _ <- string "mul("
  a <- intParser
  _ <- char ','
  b <- intParser
  _ <- char ')'
  return $ IMul a b

intParser :: Parsec Void String Int
intParser = do
  str <- takeWhile1P Nothing isDigit
  if length str > 3
    then parseError undefined
    else return $ read str

doParser :: Parsec Void String Instruction
doParser = string "do()" >> return IDo

dontParser :: Parsec Void String Instruction
dontParser = string "don't()" >> return IDont

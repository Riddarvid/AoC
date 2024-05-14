{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
module Days.Day12 (solve) where
import           AoCUtils.Days  (Solver)
import           AoCUtils.Regex (parseUnsignedInts)
import           Data.MemoTrie  (HasTrie (..), Reg, enumerateGeneric, memo2,
                                 trieGeneric, untrieGeneric)
import           GHC.Generics   (Generic)
import Control.Parallel.Strategies (Strategy, using, parListChunk, rseq)

data SpringRecord = SROperational | SRDamaged | SRUnkown
  deriving (Show, Eq, Generic)

instance HasTrie SpringRecord where
  newtype  SpringRecord :->: b = SRTrie { unSRTrie :: Reg SpringRecord :->: b }
  trie :: (SpringRecord -> b) -> SpringRecord :->: b
  trie = trieGeneric SRTrie
  untrie :: (SpringRecord :->: b) -> SpringRecord -> b
  untrie = untrieGeneric unSRTrie
  enumerate :: (SpringRecord :->: b) -> [(SpringRecord, b)]
  enumerate = enumerateGeneric unSRTrie

data Row = Row [SpringRecord] [Int]
  deriving (Show, Eq)

solve :: Solver
solve input = let
  rows = map parseRow $ lines input
  part1 = solve1 rows
  part2 = solve2 rows
  in (show part1, show part2)

parseRow :: String -> Row
parseRow line = Row (map parseRecord recordChars) (parseUnsignedInts line)
  where
    recordChars = takeWhile (/= ' ') line

parseRecord :: Char -> SpringRecord
parseRecord '.' = SROperational
parseRecord '#' = SRDamaged
parseRecord '?' = SRUnkown
parseRecord _   = error "Invalid character"

solve1 :: [Row] -> Integer
solve1 = sum . map validArrangementsRow

validArrangementsRow :: Row -> Integer
validArrangementsRow (Row springs ints) = validArrangementsMemo springs ints

validArrangementsMemo :: [SpringRecord] -> [Int] -> Integer
validArrangementsMemo = memo2 validArrangements

validArrangements :: [SpringRecord] -> [Int] -> Integer
validArrangements records []
  | SRDamaged `notElem` records = 1
  | otherwise = 0
validArrangements [] _ = 0
validArrangements rs'@(r : rs) ns'@(n : ns)
  | length (filter (/= SROperational) rs') < sum ns' = 0
  | canFit n rs' = childPlacements + alternativePlacements
  | otherwise = alternativePlacements
  where
    alternativePlacements = if r == SRDamaged
      then 0
      else validArrangementsMemo rs ns'
    childPlacements = validArrangementsMemo (drop (n + 1) rs') ns

canFit :: Int -> [SpringRecord] -> Bool
canFit 0 []       = True
canFit _ []       = False
canFit 0 (r : _)  = r /= SRDamaged
canFit n (r : rs) = r /= SROperational && canFit (n - 1) rs

solve2 :: [Row] -> Integer
solve2 rows = sum (arrangementsList `using` evalStrat)
  where
    arrangementsList = map (validArrangementsRow . unfoldRow) rows

unfoldRow :: Row -> Row
unfoldRow (Row rs ns) =
  Row (rs ++ concat (replicate 4 (SRUnkown : rs))) (concat $ replicate 5 ns)

-- Using this strategy results in ~2x speedup with 7 threads!
evalStrat :: Strategy [a]
evalStrat = parListChunk 100 rseq
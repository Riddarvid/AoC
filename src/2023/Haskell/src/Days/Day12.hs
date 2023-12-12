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
solve1 = sum . map validArrangements

validArrangements :: Row -> Integer
validArrangements (Row springs ints) = memo2 validArrangements' springs ints

validArrangements' :: [SpringRecord] -> [Int] -> Integer
validArrangements' records []
  | SRDamaged `notElem` records = 1
  | otherwise = 0
validArrangements' [] _ = 0
validArrangements' rs'@(r : rs) ns'@(n : ns)
  | length (filter (/= SROperational) rs') < sum ns' = 0
  | canFit n rs' = childPlacements + alternativePlacements
  | otherwise = alternativePlacements
  where
    alternativePlacements = if r == SRDamaged
      then 0
      else memo2 validArrangements' rs ns'
    childPlacements = memo2 validArrangements' (drop (n + 1) rs') ns

canFit :: Int -> [SpringRecord] -> Bool
canFit 0 []       = True
canFit _ []       = False
canFit 0 (r : _)  = r /= SRDamaged
canFit n (r : rs) = r /= SROperational && canFit (n - 1) rs

solve2 :: [Row] -> Integer
solve2 = sum . map (validArrangements . unfoldRow)

unfoldRow :: Row -> Row
unfoldRow (Row rs ns) =
  Row (rs ++ concat (replicate 4 (SRUnkown : rs))) (concat $ replicate 5 ns)

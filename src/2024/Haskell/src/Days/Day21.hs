{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Days.Day21 (solve) where
import           AoCUtils.Days         (Solver)
import           AoCUtils.Dijkstra     (dijkstraFull)
import           AoCUtils.Geometry     (Point (vectorBetween), Point2)
import           AoCUtils.Matrices     (matrixToMapList)
import           Data.Foldable         (find)
import           Data.Function.Memoize (deriveMemoizable, memoize2)
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Maybe            (fromJust)
import           Utils                 (Direction (..), dirVector, directions,
                                        neighborsOf, shortestPaths)

-- Idea
-- For a given code, convert it to a list of moves, always starting at A (So add an additional first position)
-- For each move, figure out the number of paths to the goal.
-- (Either do this explicitly as precomputation, or memoize the result.)
-- Bam

-- Handling indirections:
-- Let's assume that we in each level only need concern ourselves with the shortest paths.
-- I think this is tru, because each unnecessary < would add another unnecessary >, so it seems unlikely
-- that it would gain us anything.

-- For each move, find all possible shortest moves one level up.
-- for each of those moves, find the shortest moves one level up.
-- I think that we can at this point do a pruning and only keep the shortest moves for a given level.
-- I fear that if we don't do this, the problem will explode.
-- We can, however, try without the pruning first.

-- We start with one level of converting numpad moves to directional moves.
-- Then we have two levels of converting directional moves to directional moves.
-- This is actually not that bad, so we might be okay going with the naive approach.

-- We should be able to handle each move separately though, since we will always end at the first button
-- after the first move.

data NumPadChar = NA | N0 | N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9
  deriving (Eq, Ord)

data DirPadChar = DA | DLeft | DUp | DRight | DDown
  deriving (Eq, Ord)

data Move a = Move a a
  deriving (Show)

$(deriveMemoizable ''DirPadChar)

$(deriveMemoizable ''Move)

instance Show NumPadChar where
  show :: NumPadChar -> String
  show = \case
    NA -> "A"
    N0 -> "0"
    N1 -> "1"
    N2 -> "2"
    N3 -> "3"
    N4 -> "4"
    N5 -> "5"
    N6 -> "6"
    N7 -> "7"
    N8 -> "8"
    N9 -> "9"

instance Show DirPadChar where
  show :: DirPadChar -> String
  show = \case
    DA     -> "A"
    DUp    -> "^"
    DRight -> ">"
    DDown  -> "v"
    DLeft  -> "<"

type Code a = [a]

class KeyPad a where
  startKey :: a

instance KeyPad NumPadChar where
  startKey :: NumPadChar
  startKey = NA

instance KeyPad DirPadChar where
  startKey :: DirPadChar
  startKey = DA

solve :: Solver
solve input = let
  codes = parseCodes input
  part1 = solve1 codes
  part2 = solve2 codes
  in (show part1, show part2)

parseCodes :: String -> [Code NumPadChar]
parseCodes = map (map parseNumPadChar) . lines

parseNumPadChar :: Char -> NumPadChar
parseNumPadChar = \case
  'A' -> NA
  '0' -> N0
  '1' -> N1
  '2' -> N2
  '3' -> N3
  '4' -> N4
  '5' -> N5
  '6' -> N6
  '7' -> N7
  '8' -> N8
  '9' -> N9
  _ -> error "Invalid char"

parseDirPadChar :: Char -> DirPadChar
parseDirPadChar = \case
  'A' -> DA
  '<' -> DLeft
  '>' -> DRight
  '^' -> DUp
  'v' -> DDown
  _ -> error "Invalid char"

solve1 :: [Code NumPadChar] -> Int
solve1 = sum . map (complexity 2)

solve2 :: [Code NumPadChar] -> Int
solve2 = sum . map (complexity 25)

complexity :: Int -> Code NumPadChar -> Int
complexity nIndirections code = lengthIndirectMoves nIndirections code * numericPart code

numericPart :: Code NumPadChar -> Int
numericPart = read . concatMap show . init

-- No memo
-- Currently has to iterate through the list of moves. But we don't actually need the order,
-- we only need the amount of each move. So a potential speedup is to replace the list with a map from move to count.
lengthIndirectMoves :: Int -> Code NumPadChar -> Int
lengthIndirectMoves n = sum . map (lengthIndirectMove n) . codeToMoves

-- Probably doesn't need memo since it is only called O(1) times per initial code
lengthIndirectMove :: Int -> Move NumPadChar -> Int
lengthIndirectMove n move =
  minimum $ map (lengthIndirectDirToDirMoves n) potentialMovess
  where
    potentialMovess = indirectNumToDir move

-- No memo
lengthIndirectDirToDirMoves :: Int -> Code DirPadChar -> Int
lengthIndirectDirToDirMoves n = sum . map (lengthIndirectDirToDirMoveMemo n) . codeToMoves

-- Memo
-- Could special case for moves that don't move
lengthIndirectDirToDirMoveMemo :: Int -> Move DirPadChar -> Int
lengthIndirectDirToDirMoveMemo = memoize2 lengthIndirectDirToDirMove

lengthIndirectDirToDirMove :: Int -> Move DirPadChar -> Int
lengthIndirectDirToDirMove 0 _ = 1 -- A move with no indirections takes exactly one move to execute.
lengthIndirectDirToDirMove n move =
  minimum $ map (lengthIndirectDirToDirMoves (n - 1)) potentialCodes
  where
    potentialCodes = indirectDirToDir move

codeToMoves :: KeyPad a => Code a -> [Move a]
codeToMoves = go . (startKey :)
  where
    go (a : b : xs) = Move a b : go (b : xs)
    go _            = []

indirectNumToDir :: Move NumPadChar -> [Code DirPadChar]
indirectNumToDir (Move start end) = map pathToDirCode paths
  where
    startPoint = fromJust $ Map.lookup start numPointMap
    endPoint = fromJust $ Map.lookup end numPointMap
    predMap = Map.map fst $ dijkstraFull numPadNeighbors startPoint
    paths = shortestPaths predMap startPoint endPoint

indirectDirToDir :: Move DirPadChar -> [Code DirPadChar]
indirectDirToDir (Move start end) = map pathToDirCode paths
  where
    startPoint = fromJust $ Map.lookup start dirPointMap
    endPoint = fromJust $ Map.lookup end dirPointMap
    predMap = Map.map fst $ dijkstraFull dirPadNeighbors startPoint
    paths = shortestPaths predMap startPoint endPoint

numPadNeighbors :: Point2 Int -> [(Point2 Int, Int)]
numPadNeighbors = map (\p -> (p, 1)) . filter (`Map.member` numPadMap) . neighborsOf

dirPadNeighbors :: Point2 Int -> [(Point2 Int, Int)]
dirPadNeighbors = map (\p -> (p, 1)) . filter (`Map.member` dirPadMap) . neighborsOf


-- Dir codes should always end with 'A'
pathToDirCode :: [Point2 Int] -> Code DirPadChar
pathToDirCode (p1 : p2 : ps) = dirChar : pathToDirCode (p2 : ps)
  where
    diff = vectorBetween p1 p2
    dir = find (\d -> dirVector d == diff) directions
    dirChar = case dir of
      Nothing -> error "Invalid dir"
      Just dir' -> case dir' of
        North -> DUp
        East  -> DRight
        South -> DDown
        West  -> DLeft
pathToDirCode [_] = [startKey]
pathToDirCode [] = error "No moves"


flipMap :: Ord b => Map a b -> Map b a
flipMap = Map.fromList . map (\(a, b) -> (b, a)) . Map.toList

numPointMap :: Map NumPadChar (Point2 Int)
numPointMap = flipMap numPadMap

dirPointMap :: Map DirPadChar (Point2 Int)
dirPointMap = flipMap dirPadMap

numPadMap :: Map (Point2 Int) NumPadChar
numPadMap = Map.map parseNumPadChar $ Map.fromList charMapList'
  where
    (charMapList, _, _) = matrixToMapList ["789", "456", "123", "X0A"]
    charMapList' = filter (\(_, c) -> c /= 'X') charMapList

dirPadMap :: Map (Point2 Int) DirPadChar
dirPadMap = Map.map parseDirPadChar $ Map.fromList charMapList'
  where
    (charMapList, _, _) = matrixToMapList ["X^A", "<v>"]
    charMapList' = filter (\(_, c) -> c /= 'X') charMapList

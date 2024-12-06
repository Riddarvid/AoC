{-# LANGUAGE TupleSections #-}
module Days.Day5 (solve) where
import           AoCUtils.Days  (Solver)
import           AoCUtils.Regex (parseSignedInts)
import           Data.Map       (Map)
import qualified Data.Map       as Map
import           Data.Maybe     (fromJust)
import           Data.Set       (Set)
import qualified Data.Set       as Set
import           Utils          (topologicalSort)

type Update = [Int]

type PredecessorMap = Map Int (Set Int)

solve :: Solver
solve input = let
  (edges, updates) = parseInput input
  predecessorMaps = map (\update -> (buildPredecessorMap edges update, update)) updates
  part1 = solve1 predecessorMaps
  part2 = solve2 predecessorMaps
  in (show part1, show part2)

-- Parsing -----------------

parseInput :: String -> (Set (Int, Int), [Update])
parseInput input = (parseRules ruleLines, parseUpdates (tail updateLines'))
  where
    lines' = lines input
    (ruleLines, updateLines') = span (/= "") lines'

parseRules :: [String] -> Set (Int, Int)
parseRules = foldr (Set.insert . parseRule) Set.empty
  where
    parseRule rule = case parseSignedInts rule of
      [a, b] -> (a, b)
      _      -> error "Pair should have two components"

parseUpdates :: [String] -> [Update]
parseUpdates = map parseSignedInts

buildPredecessorMap :: Set (Int, Int) -> Update -> PredecessorMap
buildPredecessorMap relation update = Set.fold appendPred baseMap relation
  where
    appendPred (a, b) acc
      | Set.member a updateSet && Set.member b updateSet =
        Map.insertWith Set.union b (Set.singleton a) acc
      | otherwise = acc
    updateSet = Set.fromList update
    baseMap = Map.fromList $ map (, Set.empty) update

-- solve1

solve1 :: [(PredecessorMap, Update)] -> Int
solve1 = sum .
  map (middleNumber . snd) .
  filter (uncurry isOrdered)

solve2 :: [(PredecessorMap, Update)] -> Int
solve2 =
  sum .
  map (middleNumber . fst) .
  filter snd .
  map (uncurry sortIfNotOrdered)

-------- Utils -----------------------

isOrdered :: PredecessorMap -> Update -> Bool
isOrdered predecessorMap = not . snd . sortIfNotOrdered predecessorMap

sortIfNotOrdered :: PredecessorMap -> Update -> (Update, Bool)
sortIfNotOrdered predecessorMap update
  | update == update' = (update', False)
  | otherwise = (update', True)
  where
    update' = fromJust $ topologicalSort predecessorMap

middleNumber :: Update -> Int
middleNumber update = case drop (n `div` 2) update of
  (x : _) -> x
  _       -> error "No middle"
  where
    n = length update

module Days.Day5Alt (solve) where
import           AoCUtils.Days  (Solver)
import           AoCUtils.Regex (parseSignedInts)
import           Data.List      (sortBy)
import           Data.Map       (Map)
import qualified Data.Map       as Map
import           Data.Set       (Set)
import qualified Data.Set       as Set

-- Still slower than the first solution

type Update = [Int]

type RuleSet = Set (Int, Int)
type RuleMap = Map Int (Set Int)

solve :: Solver
solve input = let
  (ruleSet, updates) = parseInput input
  updatesAndRuleMaps = map (\update -> (update, buildRuleMap ruleSet update)) updates
  part1 = solve1 updatesAndRuleMaps
  part2 = solve2 updatesAndRuleMaps
  in (show part1, show part2)

-- Parsing and building the order relation -----------------

parseInput :: String -> (RuleSet, [Update])
parseInput input = (parseRules ruleLines, parseUpdates (tail updateLines'))
  where
    lines' = lines input
    (ruleLines, updateLines') = span (/= "") lines'

parseRules :: [String] -> RuleSet
parseRules = foldr (Set.insert . parseRule) Set.empty
  where
    parseRule rule = case parseSignedInts rule of
      [a, b] -> (a, b)
      _      -> error "Pair should have two components"

parseUpdates :: [String] -> [Update]
parseUpdates = map parseSignedInts

-------- Building the order relation --------------

buildRuleMap :: RuleSet -> Update -> RuleMap
buildRuleMap ruleSet update =
  Set.fold appendRule Map.empty ruleSet
  where
    updateSet = Set.fromList update
    appendRule (a, b) acc
      | Set.member a updateSet && Set.member b updateSet =
        Map.insertWith Set.union a (Set.singleton b) acc
      | otherwise = acc

-- solve ----------------------------------

solve1 :: [(Update, RuleMap)] -> Int
solve1 = sum . map (middleNumber . fst) . filter isOrderedByRules

solve2 :: [(Update, RuleMap)] -> Int
solve2 =
  sum .
  map (middleNumber . sortByRules) .
  filter (not . isOrderedByRules)

-------- Utils -----------------------

comparePages :: RuleMap -> Int -> Int -> Ordering
comparePages ruleMap a b
  | a == b = EQ
  | lessThanPages ruleMap a b = LT
  | otherwise = GT

lessThanPages :: RuleMap -> Int -> Int -> Bool
lessThanPages ruleMap _a b = go _a
  where
    go a
      | Set.size candidates == 0 = False
      | Set.member b candidates = True
      | otherwise = any go candidates
      where
        candidates = Map.findWithDefault Set.empty a ruleMap

isOrderedByRules :: (Update, RuleMap) -> Bool
isOrderedByRules (update, ruleMap) =
  all (uncurry $ lessThanPages ruleMap) $ zip update (tail update)

sortByRules :: (Update, RuleMap) -> Update
sortByRules (update, ruleMap) = sortBy (comparePages ruleMap) update

middleNumber :: Update -> Int
middleNumber update = case drop (n `div` 2) update of
  (x : _) -> x
  _       -> error "No middle"
  where
    n = length update

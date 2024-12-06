module Days.Day5 (solve) where
import           AoCUtils.Days  (Solver)
import           AoCUtils.Regex (parseSignedInts)
import           Data.List      (sortBy)
import           Data.Set       (Set)
import qualified Data.Set       as Set

-- Idé: Vi kommer behöva iterera igenom varje uppdatering. Låt oss därför lagra regler i
-- en hashmap från tal till lista av regler innehållande talet i högerfältet.
-- Om vi sedan går från vänster och stöter på ett tal som finns i den högra delen av en regel,
-- då blir det vänstra talet illegalt. Vi kan då lägga till det i ett set av illegala tal.
-- Processen blir alltså:
-- För varje tal:
--  Kolla om talet är lagligt. Om inte så är vi klara.
--  Kolla om talet förekommer till höger i någon regel. Om ja, lägg till motsvarande vänstertal
--  i setet av illegala tal.
-- Om inget tal har brutit mot någon regel är uppdateringen laglig.

-- Felet kommer troligen från att jag antar att jag kan bygga en total ordning.
-- Ordningsreglerna gäller dock endast för de tal som faktiskt finns med i
-- en given uppsättning. I relationen jag bygger finns många fler element med,
-- vilket leder till cykliska beteenden osv.
-- Jag måste istället bygga en relation per uppdatering.
-- Lättast är nog att använda samma metod som nu, men först filtrera ut de
-- regler som innehåller något av talen i den aktuella uppdateringen.

-- A quicker version is probably to just have a map from ints to ints
-- that are larger than it and just walk through it.

type Update = [Int]

type OrderRelation = Set (Int, Int)

solve :: Solver
solve input = let
  (order, updates) = parseInput input
  ordersAndUpdates = map (\update -> (buildOrderRelation order update, update)) updates
  part1 = solve1 ordersAndUpdates
  part2 = solve2 ordersAndUpdates
  in (show part1, show part2)

-- Parsing and building the order relation -----------------

parseInput :: String -> (OrderRelation, [Update])
parseInput input = (parseRules ruleLines, parseUpdates (tail updateLines'))
  where
    lines' = lines input
    (ruleLines, updateLines') = span (/= "") lines'

parseRules :: [String] -> OrderRelation
parseRules = foldr (Set.insert . parseRule) Set.empty
  where
    parseRule rule = case parseSignedInts rule of
      [a, b] -> (a, b)
      _      -> error "Pair should have two components"

parseUpdates :: [String] -> [Update]
parseUpdates = map parseSignedInts

-------- Building the order relation --------------

-- From the given relation, complete it using the transitive property.
buildOrderRelation :: OrderRelation -> [Int] -> OrderRelation
buildOrderRelation order members = buildOrderRelation' order' Set.empty $ Set.toList order'
  where
    order' = Set.filter (\(a, b) -> Set.member a memberSet && Set.member b memberSet) order
    memberSet = Set.fromList members

buildOrderRelation' :: OrderRelation -> Set (Int, Int) -> [(Int, Int)] -> OrderRelation
buildOrderRelation' order _ [] = order
buildOrderRelation' order examined (candidate : candidates)
  | candidate `Set.member` examined = buildOrderRelation' order examined candidates
  | otherwise = buildOrderRelation' order' examined' candidates'
  where
    newItems = mapMaybeSet (fromTransitive candidate) order
    order' = Set.union newItems order
    examined' = Set.insert candidate examined
    candidates' = Set.toList newItems ++ candidates

mapMaybeSet :: Ord b => (a -> Maybe b) -> Set a -> Set b
mapMaybeSet f = Set.fold appendJust Set.empty
  where
    appendJust a acc = case f a of
      Nothing -> acc
      Just b  -> Set.insert b acc

fromTransitive :: (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
fromTransitive (a, b) (c, d)
  | b == c = Just (a, d)
  | d == a = Just (c, b)
  | otherwise = Nothing

-- solve1

solve1 :: [(OrderRelation, Update)] -> Int
solve1 = sum .
  map (middleNumber . snd) .
  filter (uncurry isOrdered)

solve2 :: [(OrderRelation, Update)] -> Int
solve2 =
  sum .
  map (middleNumber . uncurry sortUsing) .
  filter (not . uncurry isOrdered)

-------- Utils -----------------------

isOrdered :: OrderRelation -> Update -> Bool
isOrdered orderRelation update =
  all (uncurry $ lessThanUsing orderRelation) $ zip update (tail update)

sortUsing :: OrderRelation -> Update -> Update
sortUsing order = sortBy (compareUsing order)

compareUsing :: Ord a => Set (a, a) -> a -> a -> Ordering
compareUsing relation a b
  | a == b = EQ
  | (a, b) `Set.member` relation = LT
  | otherwise = GT

lessThanUsing :: Ord a => Set (a, a) -> a -> a -> Bool
lessThanUsing relation a b = (== LT) $ compareUsing relation a b

middleNumber :: Update -> Int
middleNumber update = case drop (n `div` 2) update of
  (x : _) -> x
  _       -> error "No middle"
  where
    n = length update

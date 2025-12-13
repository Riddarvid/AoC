{-# LANGUAGE TupleSections #-}
module Days.Day8 (solve) where
import           AoCUtils.Days     (Solver)
import           AoCUtils.Geometry (Point, Point3 (P3))
import           AoCUtils.Regex    (parseSignedInts)
import           Data.Foldable     (foldl')
import           Data.List         (sortBy)
import           Data.Maybe        (fromJust)
import           Data.Ord          (comparing)
import qualified UnionFind         as UF
import           UnionFind         (UnionFind)

-- I interpret in part 1 that we should do the first 1000
-- connection attempts, even if nothing happens.
-- If we just made 1000 actual connections, it would all be one circuit
-- (might be part 2).
-- So, we should sort the pairs by distance and just connect the first 1000
-- but only the ones from that list that aren't already connected.
-- We could do this by having some union-find like structure, where
-- each node maps to some representative of the set.

type Connection = (Point3 Int, Point3 Int)

solve :: Solver
solve input = let
  boxes = parseBoxes input
  part1 = solve1 boxes
  part2 = solve2 boxes
  in (show part1, show part2)

parseBoxes :: String -> [Point3 Int]
parseBoxes = map parseBox . lines

parseBox :: String -> Point3 Int
parseBox line = case parseSignedInts line of
  [x, y, z] -> P3 x y z
  _         -> error "Couldn't parse box"

solve1 :: [Point3 Int] -> Integer
solve1 boxes = product $ map toInteger threeLargestCircuits
  where
    sortedPairs = mkSortedPairs boxes
    first1000Pairs = take 1000 sortedPairs
    initialCircuits = UF.fromList boxes
    circuitsAfter1000 = foldl' makeConnection initialCircuits first1000Pairs
    threeLargestCircuits = take 3 $ sortBy (flip compare) $ UF.setSizes circuitsAfter1000

solve2 :: [Point3 Int] -> Integer
solve2 boxes = toInteger x1 * toInteger x2
  where
    sortedConnections = mkSortedPairs boxes
    lastConnection = findLastConnection sortedConnections $ UF.fromList boxes
    (P3 x1 _ _, P3 x2 _ _) = lastConnection

makeConnection ::
  UnionFind (Point3 Int) ->
  Connection ->
  UnionFind (Point3 Int)
makeConnection uf (x, y) = fromJust $ UF.merge x y uf

mkSortedPairs :: [Point3 Int] -> [Connection]
mkSortedPairs xs = map fst sortedPairDistances
  where
    pairs = mkUniquePairs xs
    pairDistances = map (\p -> (p, uncurry euclidianDistanceBetween p :: Double)) pairs
    sortedPairDistances = sortBy (comparing snd) pairDistances

mkUniquePairs :: [a] -> [(a, a)]
mkUniquePairs []       = []
mkUniquePairs (x : xs) = map (x,) xs ++ mkUniquePairs xs

euclidianDistanceBetween :: (Point p, Floating b, Integral a) => p a -> p a -> b
euclidianDistanceBetween p1 p2 =
  sqrt $ fromIntegral $ sum (liftA2 (\a b -> abs (a - b) ^ (2 :: Integer)) p1 p2)

findLastConnection :: [Connection] -> UnionFind (Point3 Int) -> Connection
findLastConnection [] _                 = error "No connection found"
findLastConnection (connection : cs) uf = case UF.setSizes uf' of
  [_] -> connection
  _   -> findLastConnection cs uf'
  where
    uf' = makeConnection uf connection

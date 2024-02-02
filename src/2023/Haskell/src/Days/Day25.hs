{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs  #-}
module Days.Day25 (solve) where
import           AoCUtils.Days       (Solver)
import           Control.Monad.State (MonadState (state), State, evalState)
import           Data.Function       (on)
import           Data.Hashable       (Hashable)
import           Data.HashMap.Lazy   (HashMap)
import qualified Data.HashMap.Lazy   as HM
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HS
import           Data.List           (find, groupBy)
import           Data.Maybe          (fromJust)
import           GHC.Generics        (Generic)
import           System.Random       (StdGen, UniformRange, mkStdGen, uniformR)

data Edge = Edge {
  eSrc :: HashSet String,
  eDst :: HashSet String
} deriving (Eq, Generic)

instance Show Edge where
  show :: Edge -> String
  show (Edge src dest) = concat (HS.toList src) ++ " - " ++ concat (HS.toList dest)

instance Hashable Edge

solve :: Solver
solve input = let
  edges = parseInput input
  part1 = solve1 edges
  in (show part1, "")

parseInput :: String -> HashMap Edge Int
parseInput = HM.fromList . concatMap parseEntry . lines

parseEntry :: String -> [(Edge, Int)]
parseEntry line = map (\dest -> (Edge src dest, 1)) xs
  where
    tokens = filter (/= " ") $ groupBy ((==) `on` (== ' ')) line
    src = HS.singleton $ init $ head tokens
    xs = map HS.singleton $ tail tokens

-- TODO check if c == 3
solve1 :: HashMap Edge Int -> Int
solve1 edges = HS.size s1 * HS.size s2
  where
    (Edge s1 s2) = find3cut edges

find3cut :: HashMap Edge Int -> Edge
find3cut edges = fst $ fromJust $ find (\(_, n) -> n == 3) cuts
  where
    seeds = [1 ..]
    cuts = map (evalState (kargerCut edges) . mkStdGen) seeds

stUniformR :: UniformRange a => (a, a) -> State StdGen a
stUniformR = state . uniformR

kargerCut :: HashMap Edge Int -> State StdGen (Edge, Int)
kargerCut edges = case HM.size edges of
  0             -> error "Graph is empty"
  1 -> return $ head $ HM.toList edges
  _          -> do
    i <- stUniformR (0, length edges - 1)
    let ((Edge s1 s2, _), edges') = deleteAt i $ HM.toList edges
    let edges'' = HM.fromList edges'
    let s' = HS.union s1 s2
    let edges''' = replaceNode s1 s' edges''
    let edges'''' = replaceNode s2 s' edges'''
    kargerCut edges''''

-- In this step we make sure to put the newly inserted set as the destination.
replaceNode :: HashSet String -> HashSet String -> HashMap Edge Int -> HashMap Edge Int
replaceNode s s' = HM.foldrWithKey (updateMap s s') HM.empty

updateMap :: HashSet String -> HashSet String -> Edge -> Int -> HashMap Edge Int -> HashMap Edge Int
updateMap s s' edge count acc = case HM.lookup edge' acc of
  Nothing     -> HM.insert edge' count acc
  Just count' -> HM.insert edge' (count + count') acc
  where
    edge' = replaceNode' s s' edge

replaceNode' :: HashSet String -> HashSet String -> Edge -> Edge
replaceNode' s s' edge
  | eSrc edge == s = edge{eSrc = eDst edge, eDst = s'}
  | eDst edge == s = edge{eDst = s'}
  | otherwise = edge

deleteAt :: Int -> [a] -> (a, [a])
deleteAt 0 (x : xs) = (x, xs)
deleteAt i xs = (last start, init start ++ end)
  where
    (start, end) = splitAt i xs

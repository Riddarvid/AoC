{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Utils.Dijkstra (distanceTo) where

import           Data.Hashable     (Hashable)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import           Data.HashSet      (HashSet)
import qualified Data.HashSet      as HS
import           Data.Heap         (Entry (Entry), Heap)
import qualified Data.Heap         as Heap

distanceTo :: Hashable a => [a] -> (a -> Bool) -> (a -> [(a, Int)]) -> Maybe Int
distanceTo starts endCond neighborsOf = dijkstraStep endCond neighborsOf heap dists HS.empty
  where
    heap = Heap.fromList $ map (Entry 0) starts
    dists = HM.fromList $ map (, 0) starts

dijkstraStep :: forall a. Hashable a => (a -> Bool) -> (a -> [(a, Int)]) -> Heap (Entry Int a) -> HashMap a Int -> HashSet a -> Maybe Int
dijkstraStep endCond neighborsOf = dijkstraStep'
  where
    dijkstraStep' :: Heap (Entry Int a) -> HashMap a Int -> HashSet a -> Maybe Int
    dijkstraStep' heap dists visited = case Heap.viewMin heap of
      Nothing                       -> Nothing
      Just (Entry dist node, heap') -> if endCond node
        then Just dist
        else if HS.member node visited
          then dijkstraStep' heap' dists visited
          else dijkstraStep' heap'' dists' visited'
        where
          visited' = HS.insert node visited
          neighbors = neighborsOf node
          (heap'', dists') = foldr updateNeighbor (heap', dists) neighbors
          updateNeighbor :: (a, Int) -> (Heap (Entry Int a), HashMap a Int) -> (Heap (Entry Int a), HashMap a Int)
          updateNeighbor (neighbor, cost) (heap''', dists'') = case HM.lookup neighbor dists'' of
            Nothing -> newStructs
            Just oldDist -> if oldDist < newDist
              then (heap''', dists'')
              else newStructs
            where
              newDist = dist + cost
              newStructs = (Heap.insert (Entry newDist neighbor) heap''', HM.insert neighbor newDist dists'')

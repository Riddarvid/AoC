{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Utils.Dijkstra (findTarget) where

import           Control.Monad        (when)
import           Control.Monad.Except (Except, MonadError (throwError),
                                       runExcept)
import           Control.Monad.Reader (MonadReader, ReaderT (runReaderT), asks,
                                       unless)
import           Control.Monad.State  (MonadState, StateT (runStateT), gets,
                                       modify)
import           Data.Foldable        (traverse_)
import           Data.Hashable        (Hashable)
import           Data.HashMap.Lazy    (HashMap)
import qualified Data.HashMap.Lazy    as HM
import           Data.HashSet         (HashSet)
import qualified Data.HashSet         as HS
import           Data.Heap            (Entry (Entry), Heap)
import qualified Data.Heap            as Heap

data DijkstraEnv d n = DE {
  deEndCond   :: EndCond d n,
  deNeighbors :: n -> [(n, d)]
}

data DijkstraState d n = DS {
  dsHeap        :: Heap (Entry d n),
  dsDists       :: HashMap n d,
  dsVisited     :: HashSet n,
  dsLastVisited :: n
}

type EndCond d n = DijkstraState d n -> Bool

type NeighborsOf d n = n -> [(n, d)]

newtype Dijkstra d n a =
  Dijkstra (ReaderT (DijkstraEnv d n) (StateT (DijkstraState d n) (Except String)) a)
  deriving (Functor, Applicative, Monad,
  MonadState (DijkstraState d n),
  MonadReader (DijkstraEnv d n),
  MonadError String)

runDijkstra ::
  DijkstraEnv d n -> DijkstraState d n -> Dijkstra d n a -> Maybe (a, DijkstraState d n)
runDijkstra env state (Dijkstra m) = case runExcept (runStateT (runReaderT m env) state) of
  Left _err -> Nothing
  Right res -> Just res

evalDijkstra :: DijkstraEnv d n -> DijkstraState d n -> Dijkstra d n a -> Maybe a
evalDijkstra env state m = fst <$> runDijkstra env state m

execDijkstra :: DijkstraEnv d n -> DijkstraState d n -> Dijkstra d n a -> Maybe (DijkstraState d n)
execDijkstra env state m = snd <$> runDijkstra env state m

findTarget ::
  (Hashable n, Ord d, Num d) => [n] -> (n -> Bool) -> NeighborsOf d n -> Maybe (n, HashMap n d)
findTarget starts goalCond neighborsOf = do
  endState <- execDijkstra (mkEnv endCond neighborsOf) (mkState starts) dijkstraLoop
  let node = dsLastVisited endState
  let dists = dsDists endState
  return (node, dists)
  where
    endCond = mkTargetEndCond goalCond

mkTargetEndCond :: (n -> Bool) -> EndCond d n
mkTargetEndCond goal = goal . dsLastVisited

findDistances ::
  (Hashable n, Ord d, Num d) => [n] -> EndCond d n -> NeighborsOf d n -> Maybe (HashMap n d)
findDistances starts endCond neighborsOf = do
  endState <- execDijkstra (mkEnv endCond neighborsOf) (mkState starts) dijkstraLoop
  return $ dsDists endState

mkEnv :: EndCond d n -> NeighborsOf d n -> DijkstraEnv d n
mkEnv endCond neighborsOf = DE {deEndCond = endCond, deNeighbors = neighborsOf}

mkState :: (Ord d, Num d, Hashable n) => [n] -> DijkstraState d n
mkState starts = DS {
  dsHeap = Heap.fromList $ map (Entry 0) starts,
  dsDists = HM.fromList $ map (,0) starts,
  dsVisited = HS.empty,
  dsLastVisited = head starts -- Kind of hacky but should work
}

dijkstraLoop :: (Hashable n, Ord d, Num d) => Dijkstra d n ()
dijkstraLoop = do
  endCond <- asks deEndCond
  done <- gets endCond
  unless done $ do
    dijkstraStep
    dijkstraLoop

dijkstraStep :: (Hashable n, Ord d, Num d) => Dijkstra d n ()
dijkstraStep = do
  (node, distance) <- popNextNode
  visited <- gets dsVisited
  unless (HS.member node visited) $ do
    modify (\s -> s{dsLastVisited = node})
    modify (\s -> s{dsVisited = HS.insert node $ dsVisited s})
    updateNeighbors node distance

updateNeighbors :: (Hashable n, Ord d, Num d) => n -> d -> Dijkstra d n ()
updateNeighbors node distance = do
  neighbors <- asks (`deNeighbors` node)
  traverse_ (updateNeighbor distance) neighbors

-- TODO update dists and heap
updateNeighbor :: (Hashable n, Ord d, Num d) => d -> (n, d) -> Dijkstra d n ()
updateNeighbor dist (neighbor, cost) = do
  oldDist <- gets (HM.lookup neighbor . dsDists)
  case oldDist of
    Nothing       -> updateDistance neighbor newDist
    Just oldDist' -> when (newDist < oldDist') $ updateDistance neighbor newDist
  where
    newDist = dist + cost

updateDistance :: (Hashable n, Ord d) => n -> d -> Dijkstra d n ()
updateDistance node distance = do
  modify (\s -> s{dsDists = HM.insert node distance (dsDists s)})
  modify (\s -> s{dsHeap = Heap.insert (Entry distance node) (dsHeap s)})

popNextNode :: Dijkstra d n (n, d)
popNextNode = do
  heap <- gets dsHeap
  case Heap.viewMin heap of
    Nothing                       -> throwError "Out of nodes"
    Just (Entry dist node, heap') -> do
      modify (\s -> s{dsHeap = heap'})
      return (node, dist)

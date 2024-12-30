{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Utils (
  topologicalSort,
  Direction(..),
  directions,
  neighborsOf,
  neighborDirections,
  turnDirLeft,
  turnDirRight,
  moveByDir,
  dirVector,
  shortestPaths
) where
import           AoCUtils.Geometry (Point (moveBy), Point2, Vector2, downV,
                                    leftV, rightV, upV)
import           Data.Foldable     (find)
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Set          (Set)
import qualified Data.Set          as Set

topologicalSort :: (Ord a, Show a) => Map a (Set a) -> Maybe [a]
topologicalSort predss
  | Map.null predss = Just []
  | otherwise = do
    e <- fst <$> find (\(_, preds) -> Set.null preds) (Map.toList predss)
    let predss' = Map.delete e predss
    let predss'' = Map.map (Set.delete e) predss'
    es <- topologicalSort predss''
    return (e : es)

neighborsOf :: Num a => Point2 a -> [Point2 a]
neighborsOf point = map (moveBy point) [upV, rightV, downV, leftV]

data Direction = North | East | South | West
  deriving (Eq, Ord, Show)

directions :: [Direction]
directions = [North, East, South, West]

turnDirLeft :: Direction -> Direction
turnDirLeft North = West
turnDirLeft East  = North
turnDirLeft South = East
turnDirLeft West  = South

turnDirRight :: Direction -> Direction
turnDirRight North = East
turnDirRight East  = South
turnDirRight South = West
turnDirRight West  = North

dirVector :: Num a => Direction -> Vector2 a
dirVector = \case
  North -> upV
  East  -> rightV
  South -> downV
  West  -> leftV

moveByDir :: Num a => Point2 a -> Direction -> Point2 a
moveByDir p = moveBy p . dirVector

neighborDirections :: [Direction]
neighborDirections = [North, East, South, West]

shortestPaths :: forall a. Ord a => Map a [a] -> a -> a -> [[a]]
shortestPaths preMap start = go [[]]
  where
    go :: [[a]] -> a -> [[a]]
    go acc x
      | x == start = acc'
      | otherwise = case Map.lookup x preMap of
        Just preds -> concatMap (go acc') preds
        Nothing    -> error "Invalid premap"
      where
        acc' = map (x :) acc

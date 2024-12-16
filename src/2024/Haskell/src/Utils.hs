module Utils (
  topologicalSort,
  Direction(..),
  neighborsOf,
  neighborDirections,
  turnDirLeft,
  turnDirRight,
  moveByDir
) where
import           AoCUtils.Geometry (Point (moveBy), Point2, downV, leftV,
                                    rightV, upV)
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

moveByDir :: Num a => Point2 a -> Direction -> Point2 a
moveByDir p dir = moveBy p v
  where
    v = case dir of
      North -> upV
      East  -> rightV
      South -> downV
      West  -> leftV

neighborDirections :: [Direction]
neighborDirections = [North, East, South, West]

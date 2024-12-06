module Utils (topologicalSort) where
import           Data.Foldable (find)
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Set      (Set)
import qualified Data.Set      as Set

topologicalSort :: (Ord a, Show a) => Map a (Set a) -> Maybe [a]
topologicalSort predss
  | Map.null predss = Just []
  | otherwise = do
    e <- fst <$> find (\(_, preds) -> Set.null preds) (Map.toList predss)
    let predss' = Map.delete e predss
    let predss'' = Map.map (Set.delete e) predss'
    es <- topologicalSort predss''
    return (e : es)

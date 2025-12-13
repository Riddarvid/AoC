module UnionFind (
  UnionFind,
  empty,
  fromList,
  insert,
  find,
  merge,
  setSizes
) where
import           Data.Hashable     (Hashable)
import           Data.HashMap.Lazy (HashMap, (!))
import qualified Data.HashMap.Lazy as HM

-- forest maps an a to it's parent. If it is equal to its parent, then
-- it is the root.
-- representatives maps root elements to the size of their sets.
data UnionFind a = Uf {
  forest          :: HashMap a a,
  representatives :: HashMap a Int
}

-- Construction

empty :: UnionFind a
empty = Uf {forest = HM.empty, representatives = HM.empty}

fromList :: Hashable a => [a] -> UnionFind a
fromList = foldr insert empty

-- Transformation

insert :: Hashable a => a -> UnionFind a -> UnionFind a
insert x uf
  | HM.member x (forest uf) = uf
  | otherwise = uf{
    forest = HM.insert x x $ forest uf,
    representatives = HM.insert x 1 $ representatives uf
  }

-- TODO update uf as we walk along, to improve future finds
-- maybe embed in the state monad.
find :: Hashable a => a -> UnionFind a -> Maybe (a, Int)
find x uf = case HM.lookup x (forest uf) of
  Nothing -> Nothing
  Just parent -> if x == parent
    then let size = representatives uf ! x in Just (x, size)
    else find parent uf

merge :: Hashable a => a -> a -> UnionFind a -> Maybe (UnionFind a)
merge a b uf = do
  (aHead, aSize) <- find a uf
  (bHead, bSize) <- find b uf
  if aHead == bHead
    then return uf
    else do
      let mergedSize = aSize + bSize
      let (small, large) = if aSize < bSize then (aHead, bHead) else (bHead, aHead)
      return $ merge' small large mergedSize uf

-- Point small to large in the forest
-- and remove small from the list of set representatives
-- and update the size for large in representatives
-- Assumes small <= large, and both small and large in representatives
merge' :: Hashable a => a -> a -> Int -> UnionFind a -> UnionFind a
merge' small large size uf = uf{
  forest = HM.insert small large $ forest uf,
  representatives = HM.insert large size $ HM.delete small $ representatives uf
}

setSizes :: UnionFind a -> [Int]
setSizes = map snd . HM.toList . representatives

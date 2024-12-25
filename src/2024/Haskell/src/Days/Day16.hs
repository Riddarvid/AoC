{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Days.Day16 (solve) where
import           AoCUtils.Days     (Solver)
import           AoCUtils.Dijkstra (PredMap, dijkstraTarget)
import           AoCUtils.Geometry (Point2)
import           AoCUtils.Matrices (matrixToMapList)
import           Data.Foldable     (find)
import qualified Data.Map          as Map
import           Data.Maybe        (fromJust, mapMaybe)
import           Data.Set          (Set)
import qualified Data.Set          as Set
import           Utils             (Direction (East), directions, moveByDir,
                                    turnDirLeft, turnDirRight)

solve :: Solver
solve input = let
  (openTiles, start, end) = parseInput input
  predMap = buildPreMap openTiles start end
  part1 = solve1 predMap end
  part2 = solve2 predMap end
  in (show part1, show part2)

parseInput :: String -> (Set (Point2 Int), Point2 Int, Point2 Int)
parseInput input = (openTiles, start, end)
  where
    (mapList, _, _) = matrixToMapList $ lines input
    charMap = Map.fromList mapList
    openTiles = Map.keysSet $ Map.filter (/= '#') charMap
    start = fst $ fromJust $ find (\(_, c) -> c == 'S') mapList
    end = fst $ fromJust $ find (\(_, c) -> c == 'E') mapList

solve1 :: PredMap Node -> Point2 Int -> Int
solve1 predMap end = case mapMaybe (`Map.lookup` predMap) ends of
  [(_, cost)] -> cost
  _           -> error "No path found"
  where
    ends = map (\d -> (end, d)) directions

solve2 :: PredMap Node -> Point2 Int -> Int
solve2 predMap end = Set.size $ Set.map fst $ foldr (predsSet predMap) Set.empty ends
  where
    ends = filter (`Map.member` predMap) $ map (\d -> (end, d)) directions

predsSet :: Ord a => PredMap a -> a -> Set a -> Set a
predsSet predMap = go
  where
    go node predsSet'
      | Set.member node predsSet' = predsSet'
      | otherwise = foldr go predsSet'' preds
      where
        (preds, _) = fromJust $ Map.lookup node predMap
        predsSet'' = Set.insert node predsSet'

---------- Common -----------------

type Node = (Point2 Int, Direction)

buildPreMap :: Set (Point2 Int) -> Point2 Int -> Point2 Int -> PredMap Node
buildPreMap openTiles start end = dijkstraMap
  where
    neighborsFun' = neighborsFun openTiles
    dijkstraMap = dijkstraTarget neighborsFun' (\(p, _) -> p == end) (start, East)

-- You can either move forward or turn in place
neighborsFun :: Set (Point2 Int) -> Node -> [(Node, Int)]
neighborsFun openTiles (pos, dir) = filter (\((pos', _), _) -> Set.member pos' openTiles)
  [forwardMove, leftMove, rightMove]
  where
    forwardMove = ((moveByDir pos dir, dir), 1)
    leftMove = ((pos, turnDirLeft dir), 1000)
    rightMove = ((pos, turnDirRight dir), 1000)

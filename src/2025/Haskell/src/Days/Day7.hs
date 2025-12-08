{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Days.Day7 (solve) where
import           AoCUtils.Days         (Solver)
import           AoCUtils.Geometry     (Point2 (P2, p2Y))
import           AoCUtils.Matrices     (matrixToMapList)
import           Data.Foldable         (find)
import           Data.Function.Memoize (deriveMemoizable, memoize)
import qualified Data.HashMap.Lazy     as HM
import           Data.HashSet          (HashSet)
import qualified Data.HashSet          as HS
import           Data.Maybe            (fromJust)

deriveMemoizable ''Point2

solve :: Solver
solve input = let
  (sourcePos, splitters, height) = parseInput input
  part1 = solve1 sourcePos splitters height
  part2 = solve2 sourcePos splitters height
  in (show part1, show part2)

-- Returns the source position and the set of splitters
parseInput :: String -> (Point2 Int, HashSet (Point2 Int), Int)
parseInput input = (sourcePos, splitterSet , height)
  where
    (charMapList, _, height) = matrixToMapList $ lines input
    charMap = HM.fromList charMapList
    sourcePos = fst $ fromJust $ find (\(_, c) -> c == 'S') charMapList
    splitterMap = HM.map (const ()) $ HM.filter (== '^') charMap
    splitterSet = HS.fromMap splitterMap

solve1 :: Point2 Int -> HashSet (Point2 Int) -> Int -> Int
solve1 sourcePos splitterSet height =
  HS.size $ findSplitterBeamWillEncounter splitterSet height sourcePos

solve2 :: Point2 Int -> HashSet (Point2 Int) -> Int -> Integer
solve2 sourcePos splitterSet height =
  findNumberOfFutureTimelines splitterSet height sourcePos

splitBeam :: Point2 Int -> [Point2 Int]
splitBeam (P2 x y) = [P2 (x - 1) y, P2 (x + 1) y]

moveDown :: Point2 Int -> Point2 Int
moveDown (P2 x y) = P2 x (y + 1)

findSplitterBeamWillEncounter ::
  HashSet (Point2 Int) ->
  Int ->
  Point2 Int ->
  HashSet (Point2 Int)
findSplitterBeamWillEncounter splitters maxDepth = memoGo
  where
    memoGo = memoize go
    go beam
      | depth > maxDepth = HS.empty
      | otherwise = results'
      where
        depth = p2Y beam
        potentialNextBeam = moveDown beam
        nextBeams = if HS.member potentialNextBeam splitters
          then splitBeam beam
          else [potentialNextBeam]
        results = HS.unions $ map memoGo nextBeams
        results' = if HS.member potentialNextBeam splitters
          then HS.insert potentialNextBeam results
          else results

findNumberOfFutureTimelines ::
  HashSet (Point2 Int) ->
  Int ->
  Point2 Int ->
  Integer
findNumberOfFutureTimelines splitters maxDepth = memoGo
  where
    memoGo = memoize go
    go beam
      | depth > maxDepth = 1
      | otherwise = result
      where
        depth = p2Y beam
        potentialNextBeam = moveDown beam
        nextBeams = if HS.member potentialNextBeam splitters
          then splitBeam beam
          else [potentialNextBeam]
        result = sum $ map memoGo nextBeams

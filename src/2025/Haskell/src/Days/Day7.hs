module Days.Day7 (solve) where
import           AoCUtils.Days         (Solver)
import           AoCUtils.Geometry     (Point2 (P2, p2Y))
import           AoCUtils.Matrices     (matrixToMapList)
import           Data.Foldable         (find)
import           Data.Function.Memoize (memoFix)
import qualified Data.HashMap.Lazy     as HM
import           Data.HashSet          (HashSet)
import qualified Data.HashSet          as HS
import           Data.Maybe            (fromJust)
import           Data.Monoid           (Endo (Endo, appEndo))
import           TemplateHaskell       ()
import           Utils                 (generalRecursionStep)

solve :: Solver
solve input = let
  (sourcePos, splitters, height) = parseInput input
  part1 = solve1 splitters height sourcePos
  part2 = solve2 splitters height sourcePos
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

solve1 :: HashSet (Point2 Int) -> Int -> Point2 Int -> Int
solve1 splitterSet height = HS.size . memoFix recStep
  where
    recStep = appEndo $ findEncounteredSplittersStep splitterSet height

solve2 :: HashSet (Point2 Int) -> Int -> Point2 Int -> Integer
solve2 splitterSet height =
  memoFix $ appEndo $ countFutureTimelinesStep splitterSet height

findEncounteredSplittersStep ::
  HashSet (Point2 Int) ->
  Int ->
  Endo (Point2 Int -> HashSet (Point2 Int))
findEncounteredSplittersStep splitters maxDepth =
  Endo $ generalRecursionStep stopCond splitProblem combineSolutions
  where
    stopCond = mkAboveMaxDepthCond maxDepth HS.empty
    splitProblem = nextBeams splitters
    combineSolutions beam solutions
      | HS.member potentialNextBeam splitters = HS.insert potentialNextBeam result
      | otherwise = result
      where
        potentialNextBeam = moveDown beam
        result = HS.unions solutions

countFutureTimelinesStep ::
  HashSet (Point2 Int) ->
  Int ->
  Endo (Point2 Int -> Integer)
countFutureTimelinesStep splitters maxDepth =
  Endo $ generalRecursionStep stopCond splitProblem combineSolutions
  where
    stopCond = mkAboveMaxDepthCond maxDepth 1
    splitProblem = nextBeams splitters
    combineSolutions _ = sum

nextBeams :: HashSet (Point2 Int) -> Point2 Int -> [Point2 Int]
nextBeams splitters beam
  | HS.member potentialNextBeam splitters = splitBeam beam
  | otherwise = [potentialNextBeam]
  where
    potentialNextBeam = moveDown beam

mkAboveMaxDepthCond :: Int -> a -> (Point2 Int -> Maybe a)
mkAboveMaxDepthCond maxDepth condResult beam
  | p2Y beam > maxDepth = Just condResult
  | otherwise = Nothing

splitBeam :: Point2 Int -> [Point2 Int]
splitBeam (P2 x y) = [P2 (x - 1) y, P2 (x + 1) y]

moveDown :: Point2 Int -> Point2 Int
moveDown (P2 x y) = P2 x (y + 1)

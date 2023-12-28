module Days.Day18 (solve) where
import           AoCUtils.Days      (Solver)
import           AoCUtils.Geometry  (Point (moveBy, scaleBy),
                                     Point2 (P2, p2X, p2Y), Vector2, downV,
                                     findDimensions, leftV, rightV, upV)
import           AoCUtils.Graphs    (BfsState (bfsPreMap), Goal (GFull),
                                     bfsExplore)
import           AoCUtils.Show      (showPoints)
import           Data.Foldable      (foldl')
import           Data.Function      (on)
import qualified Data.HashMap.Lazy  as HM
import           Data.HashSet       (HashSet)
import qualified Data.HashSet       as HS
import           Data.List          (elemIndex, intersect, nub, sort, sortOn,
                                     (\\))
import           Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe         (fromJust)
import           Debug.Trace        (trace, traceShow, traceShowId)

type Pos = Point2 Int

data Dir = DUp | DLeft | DDown | DRight
  deriving (Show)

data Instruction = Instr Dir Int String
  deriving (Show)

solve :: Solver
solve input = let
  instrs = NE.map parseInstr $ NE.fromList $ lines input
  part1 = solve1 instrs
  in (show part1, "")

parseInstr :: String -> Instruction
parseInstr input = Instr dir (read l) (drop 1 color')
  where
    dir = case head input of
      'U' -> DUp
      'L' -> DLeft
      'D' -> DDown
      'R' -> DRight
      _   -> undefined
    (l, color') = span (/= ' ') $ drop 2 input

solve1 :: NonEmpty Instruction -> Integer
solve1 = solveGeneral

digEdge :: [Instruction] -> HashSet Pos
digEdge = snd . foldl' (uncurry digInstr) (P2 0 0, HS.empty)

digInstr :: Pos -> HashSet Pos -> Instruction -> (Pos, HashSet Pos)
digInstr p edge (Instr dir length' _) = (p', HS.union edge $ HS.fromList edge')
  where
    dirV' = dirV dir
    edge' = [p `moveBy` (dirV' `scaleBy` s) | s <- [1 .. length']]
    p' = p `moveBy` (dirV' `scaleBy` length')

dirV :: Dir -> Vector2 Int
dirV DUp    = upV
dirV DLeft  = leftV
dirV DDown  = downV
dirV DRight = rightV

solveGeneral :: NonEmpty Instruction -> Integer
solveGeneral = calcArea . findCorners

findCorners :: NonEmpty Instruction -> NonEmpty Pos
findCorners = NE.fromList
  . NE.tail
  . foldl' (\acc@(p :| _) v -> (p `moveBy` v) <| acc) (P2 0 0 :| [])
  . NE.map instrToVec

instrToVec :: Instruction -> Vector2 Int
instrToVec (Instr dir mag _) = dirV dir `scaleBy` mag

formYGroups :: NonEmpty Pos -> NonEmpty (Int, NonEmpty Int)
formYGroups =
  NE.map (\ps -> (p2X $ NE.head ps, NE.map p2Y ps)) . NE.groupBy1 ((==) `on` p2X) . NE.sortWith p2X

findDistances :: NonEmpty (Int, NonEmpty Int) -> [Int]
findDistances (_ :| [])     = []
findDistances (a :| b : xs) = (fst b - fst a) : findDistances (b :| xs)

calcArea :: NonEmpty Pos -> Integer
calcArea corners = foldl' (\acc entry -> acc + calcAreaSegment entry) 0 $ zip dists segmentYs
  where
    yGroupsNE = formYGroups corners
    yGroups = map (sort . NE.toList . snd) $ NE.toList yGroupsNE
    dists = 0 : 1 : foldr (\d acc -> (d - 1) : 1 : acc) [] (findDistances yGroupsNE)
    segmentYs = map sort $ snd $ foldr continuingYs ([], []) yGroups

-- ys is sorted
continuingYs :: [Int] -> ([Int], [[Int]]) -> ([Int], [[Int]])
continuingYs ys (right', acc) = (left', left' : here : acc)
  where
    here = mergeRanges $ sortOn fst (makePairs left' ++ makePairs right')
    left' = sort $ (right' \\ ys) ++ (ys \\ right')

mergeRanges :: [(Int, Int)] -> [Int]
mergeRanges []                     = []
mergeRanges [(a, b)]               = [a, b]
mergeRanges ((a, b) : (c, d) : xs)

  | b >= c = mergeRanges ((min a c, max b d) : xs)
  | otherwise = a : b : mergeRanges ((c, d) : xs)

makePairs :: [Int] -> [(Int, Int)]
makePairs []           = []
makePairs (a : b : xs) = (a, b) : makePairs xs
makePairs _            = error "Must be even"

-- ys must be sorted
calcAreaSegment :: (Int, [Int]) -> Integer
calcAreaSegment (xDist, ys) = x * y
  where
    x = toInteger xDist
    y = findYDistance ys

findYDistance :: [Int] -> Integer
findYDistance [] = 0
findYDistance (a : b : ys) = b' - a' + 1 + findYDistance ys
  where
    a' = toInteger a
    b' = toInteger b
findYDistance _ = error "Number of ys must be evenly divisible by 2"

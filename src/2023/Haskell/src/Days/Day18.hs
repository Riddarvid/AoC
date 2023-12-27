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
import           Data.List          (intersect, sortOn, (\\))
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe         (fromJust)
import           Debug.Trace        (trace, traceShowId)

type Pos = Point2 Int

data Dir = DUp | DLeft | DDown | DRight
  deriving (Show)

data Instruction = Instr Dir Int String
  deriving (Show)

solve :: Solver
solve input = let
  instrs = map parseInstr $ lines input
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

findInsideSize :: HashSet Pos -> Int
findInsideSize edge = length inside
  where
    (minX, minY, maxX, maxY) = findDimensions edge
    bfs = fromJust $
      bfsExplore (P2 (minX - 1) (minY - 1)) GFull (mkAdjacency minX minY maxX maxY edge)
    outside = HS.insert (P2 (minX - 1) (minY - 1)) $ HM.keysSet $ bfsPreMap bfs
    inside = filter (\p -> not $ HS.member p outside)
      [P2 x y | x <- [minX - 1 .. maxX + 1], y <- [minY - 1 .. maxY + 1]]

mkAdjacency :: Int -> Int -> Int -> Int -> HashSet Pos -> Pos -> [Pos]
mkAdjacency minX minY maxX maxY edge p = neighbors''
  where
    neighbors = map (moveBy p . dirV) [DUp, DLeft, DDown, DRight]
    neighbors' = filter (\(P2 x y) -> minX - 1 <= x && x <= maxX + 1 && minY - 1 <= y && y <= maxY + 1) neighbors
    neighbors'' = filter (\p' -> not $ HS.member p' edge) neighbors'


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
findCorners = undefined

-- Returns a list of groups,
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
    dists = findDistances yGroupsNE
    yGroups = map snd $ NE.toList yGroupsNE
    segmentYs = snd $ foldl' continuingYs ([], []) $ map NE.toList yGroups

continuingYs :: ([Int], [[Int]]) -> [Int] -> ([Int], [[Int]])
continuingYs (current, acc) ys = (ys', ys' : acc)
  where
    ys' = (current ++ ys) \\ intersect current ys

-- ys are sorted
calcAreaSegment :: (Int, [Int]) -> Integer
calcAreaSegment (dist, ys) = _

module Days.Day10 (
  solve,
  findInsideScaled,
  scaleDownSet,
  scaleUpMap,
  scaleUpStart,
  parseInput,
  exploreLoop,
  loopTiles,
  Pos,
  Tile(..)
) where
import           AoCUtils.Days     (Solver)
import           AoCUtils.Geometry (Point (scaleBy), Point2 (P2), downV, leftV,
                                    moveBy, rightV, upV)
import           AoCUtils.Graphs   (BfsState (bfsNLayers, bfsPreMap),
                                    Goal (GFull), bfsExplore)
import           AoCUtils.Matrices (matrixToHashMap)
import           Data.Foldable     (find)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import           Data.HashSet      (HashSet)
import qualified Data.HashSet      as HS
import           Data.List         (intersect, sort)
import           Data.Maybe        (fromJust)

type Pos = Point2 Int

data Tile = NS | WE | NE | NW | SW | SE | Start | Empty
  deriving (Eq)

data Direction = DUp | DLeft | DDown | DRight
  deriving (Eq, Ord)

solve :: Solver
solve input = let
  (start, pipes, maxX, maxY) = parseInput input
  part1 = solve1 start pipes
  part2 = solve2 start pipes maxX maxY
  in (show part1, show part2)

parseInput :: String -> (Pos, HashMap Pos Tile, Int, Int)
parseInput input = (start, pipes', maxX, maxY)
  where
    (chars, maxX, maxY) = matrixToHashMap $ lines input
    start = fst $ fromJust $ find (\(_, c) -> c == 'S') $ HM.toList chars
    pipes = HM.map parseTile chars
    pipes' = HM.insert start (determineTile start pipes) pipes

determineTile :: Pos -> HashMap Pos Tile -> Tile
determineTile start tiles = case facings of
  [DUp, DDown]    -> NS
  [DLeft, DRight] -> WE
  [DUp, DRight]   -> NE
  [DUp, DLeft]    -> NW
  [DLeft, DDown]  -> SW
  [DDown, DRight] -> SE
  _               -> error "Invalid pipe configuration"
  where
    facings = sort $ map match $
      filter (== DDown) (faces' $ moveBy upV start) ++
      filter (== DRight) (faces' $ moveBy leftV start) ++
      filter (== DUp) (faces' $ moveBy downV start) ++
      filter (== DLeft) (faces' $ moveBy rightV start)
    faces' :: Pos -> [Direction]
    faces' pos = maybe [] faces (HM.lookup pos tiles)

parseTile :: Char -> Tile
parseTile '|' = NS
parseTile '-' = WE
parseTile 'L' = NE
parseTile 'J' = NW
parseTile '7' = SW
parseTile 'F' = SE
parseTile 'S' = Start
parseTile '.' = Empty
parseTile _   = error "Invalid tile"

solve1 :: Pos -> HashMap Pos Tile -> Integer
solve1 start pipes = bfsNLayers (exploreLoop start pipes) - 1

exploreLoop :: Pos -> HashMap Pos Tile -> BfsState Pos
exploreLoop start = fromJust . bfsExplore start GFull . loopAdjacencies

-- If both pipes face eachother, add the new one
loopAdjacencies :: HashMap Pos Tile -> Pos -> [Pos]
loopAdjacencies pipes p = filter (areConnected pipes p) neighbors
  where
    neighbors = pipeNeighbors pipes p

pipeNeighbors :: HashMap Pos Tile -> Pos -> [Pos]
pipeNeighbors tiles pos = case HM.lookup pos tiles of
  Nothing   -> error "Should not happen"
  Just tile -> map ((`moveBy` pos) . dirToVec) $ faces tile

dirToVec :: Direction -> Pos
dirToVec DUp    = upV
dirToVec DLeft  = leftV
dirToVec DDown  = downV
dirToVec DRight = rightV

areConnected :: HashMap Pos Tile -> Pos -> Pos -> Bool
areConnected pipes p1 p2 = case HM.lookup p1 pipes of
  Nothing -> False
  Just t1 -> case HM.lookup p2 pipes of
    Nothing -> False
    Just t2 -> areConnected' t1 t2

areConnected' :: Tile -> Tile -> Bool
areConnected' t1 t2 = not $ null $ intersect (faces t1) $ map match $ faces t2

match :: Direction -> Direction
match DUp    = DDown
match DLeft  = DRight
match DDown  = DUp
match DRight = DLeft

faces :: Tile -> [Direction]
faces Empty = []
faces Start = [DUp, DLeft, DDown, DRight]
faces NS    = [DUp, DDown]
faces WE    = [DLeft, DRight]
faces NE    = [DUp, DRight]
faces NW    = [DUp, DLeft]
faces SW    = [DDown, DLeft]
faces SE    = [DDown, DRight]

solve2 :: Pos -> HashMap Pos Tile -> Int -> Int -> Int
solve2 start tiles maxX maxY = length inside
  where
    insideScaled = findInsideScaled start tiles maxX maxY
    inside = scaleDownSet insideScaled

loopTiles :: Pos -> BfsState Pos -> HashSet Pos
loopTiles start bfs = HS.insert start $ HM.keysSet $ bfsPreMap bfs

findInsideScaled :: Pos -> HashMap Pos Tile -> Int -> Int -> HashSet Pos
findInsideScaled start tiles maxX maxY = filterInside outside loop tiles''
  where
    tiles' = scaleUpMap tiles
    tiles'' = extendBorder tiles' (maxX * 2 + 1) (maxY * 2 + 1)
    start' = scaleUpStart start
    loop = loopTiles start' $ exploreLoop start' tiles''
    outside = findOutsidePoints tiles'' loop

scaleDownSet :: HashSet Pos -> HashSet Pos
scaleDownSet = HS.filter (\(P2 x' y') -> even x' && even y')

-- Scales each point x2 depending on the pipe it corresponds to
scaleUpMap :: HashMap Pos Tile -> HashMap Pos Tile
scaleUpMap = HM.unions . map (uncurry scalePoint) . HM.toList

scaleUpStart :: Pos -> Pos
scaleUpStart pos = scaleBy pos 2

scalePoint :: Pos -> Tile -> HashMap Pos Tile
scalePoint (P2 x' y') tile = HM.fromList $
  zip [P2 x y, P2 (x + 1) y, P2 x (y + 1), P2 (x + 1) (y + 1)] points
  where
    x = x' * 2
    y = y' * 2
    points = case tile of
      Empty -> [Empty, Empty, Empty, Empty]
      NS    -> [NS, Empty, NS, Empty]
      WE    -> [WE, WE, Empty, Empty]
      NE    -> [NE, WE, Empty, Empty]
      NW    -> [NW, Empty, Empty, Empty]
      SW    -> [SW, Empty, NS, Empty]
      SE    -> [SE, WE, NS, Empty]
      _     -> error "Should not happen"

-- Adds a border of empty spaces around the map
-- Works since union prioritizes the values from the first map
extendBorder :: HashMap Pos Tile -> Int -> Int -> HashMap Pos Tile
extendBorder tiles maxX maxY =
  HM.union tiles $ HM.fromList $
   [(P2 x (-1), Empty) | x <- [-1 .. maxX + 1]] ++
   [(P2 x (maxY + 1), Empty) | x <- [-1 .. maxX + 1]] ++
   [(P2 (-1) y, Empty) | y <- [-1 .. maxY + 1]] ++
   [(P2 (maxX + 1) y, Empty) | y <- [-1 .. maxY + 1]]

-- Starting at the top left corner, searches from (-1, -1)
findOutsidePoints :: HashMap Pos Tile -> HashSet Pos -> HashSet Pos
findOutsidePoints tiles scaledLoop =
  HS.insert start $ HM.keysSet $ bfsPreMap bfsState
  where
    start = P2 (-1) (-1)
    bfsState = fromJust $ bfsExplore start GFull $ outsideAdjecency tiles scaledLoop

-- Pos and loop have scaled coordinates, tiles and maxes have normal
outsideAdjecency :: HashMap Pos Tile -> HashSet Pos -> Pos -> [Pos]
outsideAdjecency tiles loop pos = neighbors''
  where
    neighbors = getNeighbors pos
    neighbors' = filter (\pos' -> not $ HS.member pos' loop) neighbors
    neighbors'' = filter (`HM.member` tiles) neighbors'

getNeighbors :: Pos -> [Pos]
getNeighbors pos = map (`moveBy` pos) [upV, leftV, downV, rightV]

filterInside :: HashSet Pos -> HashSet Pos -> HashMap Pos Tile -> HashSet Pos
filterInside outside loop = HM.keysSet . HM.filterWithKey (isInside outside loop)

-- Assumes both outside and loop are scaled x2
isInside :: HashSet Pos -> HashSet Pos -> Pos -> Tile -> Bool
isInside outside loop pos _
  | HS.member pos outside = False
  | HS.member pos loop = False
  | otherwise = True

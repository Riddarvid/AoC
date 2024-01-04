module Days.Day23 (solve) where
import           AoCUtils.Days     (Solver)
import           AoCUtils.Geometry (Point (moveBy), Point2 (P2), Vector2, downV,
                                    leftV, rightV, upV)
import           AoCUtils.Matrices (matrixToHashMap)
import           Data.Foldable     (find)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import           Data.HashSet      (HashSet)
import qualified Data.HashSet      as HS
import           Data.Maybe        (catMaybes, fromJust, mapMaybe)

type Pos = Point2 Int

data Dir = DUp | DLeft | DDown | DRight
  deriving (Eq, Show)

dirV :: Dir -> Vector2 Int
dirV DUp    = upV
dirV DLeft  = leftV
dirV DDown  = downV
dirV DRight = rightV

turnLeft :: Dir -> Dir
turnLeft DUp    = DLeft
turnLeft DLeft  = DDown
turnLeft DDown  = DRight
turnLeft DRight = DUp

turnRight :: Dir -> Dir
turnRight = turnLeft . turnLeft . turnLeft

data Tile = Forest | Path | Slope Dir
  deriving (Eq, Show)

type AdjacencyGraph = HashMap Pos [(Pos, Int)]

-- Observation: There are only 36 intersections, including the start and goal tile.
-- Therefore, we can create an adjacency list with 36 nodes and at most four edges
-- per node. We can then do a much faster search through this graph.
solve :: Solver
solve input = let
  (tiles, start, goal) = parseInput input
  part1 = solve1 tiles start goal
  part2 = solve2 tiles start goal
  in (show part1, show part2)

parseInput :: String -> (HashMap Pos Tile, Pos, Pos)
parseInput input = (tiles, start, goal)
  where
    (chars, _, maxY) = matrixToHashMap $ lines input
    tiles = HM.map parseTile chars
    start = head $ HM.keys $ HM.filterWithKey (\(P2 _ y) v -> y == 0 && v /= Forest) tiles
    goal = head $ HM.keys $ HM.filterWithKey (\(P2 _ y) v -> y == maxY && v /= Forest) tiles

parseTile :: Char -> Tile
parseTile '#' = Forest
parseTile '.' = Path
parseTile '^' = Slope DUp
parseTile '<' = Slope DLeft
parseTile 'v' = Slope DDown
parseTile '>' = Slope DRight
parseTile _   = error "Invalid char"

solve1 :: HashMap Pos Tile -> Pos -> Pos -> Int
solve1 tiles start goal = fromJust $ longestPathTo tiles goal start DDown

longestPathTo :: HashMap Pos Tile -> Pos -> Pos -> Dir -> Maybe Int
longestPathTo tiles goal pos = longestPathTo' tiles goal (HS.singleton pos) pos

longestPathTo' :: HashMap Pos Tile -> Pos -> HS.HashSet Pos -> Pos -> Dir -> Maybe Int
longestPathTo' tiles goal = go
  where
    go :: HS.HashSet Pos -> Pos -> Dir -> Maybe Int
    go _ pos _ | pos == goal = Just 0
    go visited pos dir = case catMaybes subDists of
      []        -> Nothing
      subDists' -> Just (1 + maximum subDists')
      where
        visited' = HS.insert pos visited
        subDists = map (uncurry $ go visited') $ getNeighbors tiles visited' pos dir

getNeighbors :: HashMap Pos Tile -> HS.HashSet Pos -> Pos -> Dir -> [(Pos, Dir)]
getNeighbors tiles visited pos dir = case HM.lookup pos tiles of
  Nothing -> []
  Just tile -> neighbors'
    where
      dirs = case tile of
        Forest         -> []
        Path           -> [dir, turnLeft dir, turnRight dir]
        Slope slopeDir -> [slopeDir]
      neighbors = map (\d -> (pos `moveBy` dirV d, d)) dirs
      neighbors' = filter (not . (`HS.member` visited) . fst) neighbors

solve2 :: HashMap Pos Tile -> Pos -> Pos -> Int
solve2 tiles start goal = longestPath2 adjacencyGraph goal start
  where
    tiles' = HM.map slopeToPath tiles

    slopeToPath :: Tile -> Tile
    slopeToPath (Slope _) = Path
    slopeToPath tile      = tile

    adjacencyGraph = buildAdjacencyGraph tiles'

longestPath2 :: AdjacencyGraph -> Pos -> Pos -> Int
longestPath2 adjacencyGraph goal start = fromJust $
  longestPath2' adjacencyGraph goal (HS.singleton start) start

longestPath2' :: AdjacencyGraph -> Pos -> HashSet Pos -> Pos -> Maybe Int
longestPath2' adjacencyGraph goal = go
  where
    go :: HashSet Pos -> Pos -> Maybe Int
    go _ pos | pos == goal = Just 0
    go visited pos = case subDists of
      [] -> Nothing
      _  -> Just $ maximum subDists
      where
        visited' = HS.insert pos visited
        neighbors = fromJust $ HM.lookup pos adjacencyGraph
        neighbors' = filter (\(p, _) -> not $ HS.member p visited') neighbors
        subDists = mapMaybe (\(p, n) -> (+ n) <$> go visited' p) neighbors'

findCrossroads :: HashMap Pos Tile -> HashSet Pos
findCrossroads tiles = HM.keysSet $ HM.filterWithKey (isCrossroad tiles) tiles

isCrossroad :: HashMap Pos Tile -> Pos -> Tile -> Bool
isCrossroad _ _ Forest = False
isCrossroad tiles pos _ = length neighbors'' /= 2
  where
    neighbors = map (pos `moveBy`) [upV, leftV, rightV, downV]
    neighbors' = mapMaybe (`HM.lookup` tiles) neighbors
    neighbors'' = filter (/= Forest) neighbors'

buildAdjacencyGraph :: HashMap Pos Tile -> AdjacencyGraph
buildAdjacencyGraph tiles =
  HM.fromList $ map (\p -> (p, crossroadNeighbors crossroads tiles p)) $ HS.toList crossroads
  where
    crossroads = findCrossroads tiles

crossroadNeighbors :: HashSet Pos -> HashMap Pos Tile -> Pos -> [(Pos, Int)]
crossroadNeighbors crossroads tiles pos =
  map (uncurry $ findNextCrossroad crossroads tiles) starts'
  where
    starts = map (\dir -> (pos `moveBy` dirV dir, dir)) [DUp, DLeft, DDown, DRight]
    starts' = filter (\(p, _) -> isWalkable tiles p) starts

isWalkable :: HashMap Pos Tile -> Pos -> Bool
isWalkable tiles pos = case HM.lookup pos tiles of
  Nothing   -> False
  Just tile -> tile /= Forest

findNextCrossroad :: HashSet Pos -> HashMap Pos Tile -> Pos -> Dir -> (Pos, Int)
findNextCrossroad crossroads _ pos _ | HS.member pos crossroads = (pos, 1)
findNextCrossroad crossroads tiles pos dir = (goal, n + 1)
  where
    dirs = [dir, turnLeft dir, turnRight dir]
    nexts = map (\d -> (pos `moveBy` dirV d, d)) dirs
    next = fromJust $ find (\(p, _) -> isWalkable tiles p) nexts
    (goal, n) = uncurry (findNextCrossroad crossroads tiles) next

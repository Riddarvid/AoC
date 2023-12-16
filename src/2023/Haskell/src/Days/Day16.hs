{-# LANGUAGE DeriveGeneric #-}
module Days.Day16 (solve) where
import           AoCUtils.Days     (Solver)
import           AoCUtils.Geometry (Point (moveBy), Point2 (P2), Vector2, downV,
                                    leftV, rightV, upV)
import           AoCUtils.Graphs   (BfsState (bfsPreMap), Goal (GFull),
                                    bfsExplore)
import           AoCUtils.Matrices (matrixToHashMap)
import           Data.Hashable     (Hashable)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet      as HS
import           Data.Maybe        (fromJust)
import           GHC.Generics      (Generic)

data Tile = Empty | Slash | Backslash | VertSplitter | HorSplitter

data Dir = DUp | DLeft | DDown | DRight
  deriving(Eq, Generic)

instance Hashable Dir

type Pos = Point2 Int

type TileMap = HashMap Pos Tile

solve :: Solver
solve input = let
  (charMap, maxX, maxY) = matrixToHashMap $ lines input
  tileMap = HM.map parseTile charMap
  part1 = solve1 tileMap
  part2 = solve2 tileMap maxX maxY
  in (show part1, show part2)

parseTile :: Char -> Tile
parseTile '.'  = Empty
parseTile '/'  = Slash
parseTile '\\' = Backslash
parseTile '|'  = VertSplitter
parseTile '-'  = HorSplitter
parseTile _    = error "Invalid tile"

solve1 :: TileMap -> Int
solve1 tiles = countEnergized tiles (P2 0 0, DRight)

solve2 :: TileMap -> Int -> Int -> Int
solve2 tiles maxX maxY = maximum $ map (countEnergized tiles) $
  [(P2 0 y, DRight)   | y <- [0 .. maxY]] ++
  [(P2 maxX y, DLeft) | y <- [0 .. maxY]] ++
  [(P2 x 0, DDown)    | x <- [0 .. maxX]] ++
  [(P2 x maxY, DUp)   | x <- [0 .. maxX]]

mkAdjacency :: TileMap -> (Pos, Dir) -> [(Pos, Dir)]
mkAdjacency tiles (pos, dir) = case HM.lookup pos tiles of
  Nothing -> error "Should not happen"
  Just tile -> filter (\(pos', _) -> HM.member pos' tiles) neighbors
    where
      dirs = newDirs dir tile
      neighbors = map (\dir' -> (pos `moveBy` dirV dir', dir')) dirs

newDirs :: Dir -> Tile -> [Dir]
newDirs dir tile = case tile of
  Empty -> [dir]
  Slash -> case dir of
    DUp    -> [DRight]
    DLeft  -> [DDown]
    DDown  -> [DLeft]
    DRight -> [DUp]
  Backslash -> case dir of
    DUp    -> [DLeft]
    DLeft  -> [DUp]
    DDown  -> [DRight]
    DRight -> [DDown]
  VertSplitter -> case dir of
    DUp   -> [dir]
    DDown -> [dir]
    _     -> [DUp, DDown]
  HorSplitter -> case dir of
    DLeft  -> [dir]
    DRight -> [dir]
    _      -> [DLeft, DRight]

dirV :: Dir -> Vector2 Int
dirV DUp    = upV
dirV DLeft  = leftV
dirV DDown  = downV
dirV DRight = rightV

countEnergized :: TileMap -> (Pos, Dir) -> Int
countEnergized tiles start = length energized
  where
    bfs = fromJust $ bfsExplore start GFull (mkAdjacency tiles)
    energized = HS.map fst $ HS.insert start $ HM.keysSet (bfsPreMap bfs)

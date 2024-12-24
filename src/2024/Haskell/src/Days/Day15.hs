{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE InstanceSigs #-}
module Days.Day15 (solve) where
import           AoCUtils.Days     (Solver)
import           AoCUtils.Geometry (Point2 (P2))
import           AoCUtils.Matrices (matrixToMapList)
import           Data.Foldable     (find)
import           Data.Function     (on)
import           Data.List         (groupBy, sortOn)
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Maybe        (fromJust)
import           Utils             (Direction (East, North, South, West),
                                    moveByDir)

solve :: Solver
solve input = let
  ((tileMap, start), (wideTileMap, wideStart), instructions) = parseInput input
  part1 = solve1 tileMap start instructions
  part2 = solve2 wideTileMap wideStart instructions
  in (show part1, show part2)

data SingleTile = SWall | SBox | SEmpty
  deriving (Eq, Show)

data WideTile = WWall | WLeftBox | WRightBox | WEmpty
  deriving (Eq, Show)

type PosMap a = Map (Point2 Int) a

parseInput :: [Char] -> ((PosMap SingleTile, Point2 Int), (PosMap WideTile, Point2 Int), [Direction])
parseInput input = (parseTileMap mapLines, parseWideTileMap mapLines, instructions)
  where
    (mapLines, rest) = span (/= "") $ lines input
    instrLines = tail rest
    instructions = map parseDirection $ concat instrLines

parseTileMap :: [String] -> (PosMap SingleTile, Point2 Int)
parseTileMap mapLines = (tileMap, start)
  where
    (mapList, _, _) = matrixToMapList mapLines
    charMap = Map.insert start '.' $ Map.fromList mapList
    tileMap = Map.map parseTile charMap
    start = fst $ fromJust $ find (\(_, c) -> c == '@') mapList

parseWideTileMap :: [String] -> (PosMap WideTile, Point2 Int)
parseWideTileMap mapLines = (tileMap, start)
  where
    mapLines' = map (concatMap expandTile) mapLines
    (mapList, _, _) = matrixToMapList mapLines'
    charMap = Map.insert start '.' $ Map.fromList mapList
    tileMap = Map.map parseWideTile charMap
    start = fst $ fromJust $ find (\(_, c) -> c == '@') mapList

expandTile :: Char -> [Char]
expandTile '#' = "##"
expandTile 'O' = "[]"
expandTile '.' = ".."
expandTile '@' = "@."
expandTile _   = error "Invalid char"

parseTile :: Char -> SingleTile
parseTile '#' = SWall
parseTile 'O' = SBox
parseTile '.' = SEmpty
parseTile c   = error ("Invalid char: " ++ [c])

parseWideTile :: Char -> WideTile
parseWideTile '#' = WWall
parseWideTile '[' = WLeftBox
parseWideTile ']' = WRightBox
parseWideTile '.' = WEmpty
parseWideTile c   = error ("Invalid char: " ++ [c])

parseDirection :: Char -> Direction
parseDirection '^' = North
parseDirection '>' = East
parseDirection 'v' = South
parseDirection '<' = West
parseDirection c   = error ("Invalid char: " ++ [c])

---- Part 1 ---------------------

instance Tile SingleTile where
  empty :: SingleTile
  empty = SEmpty
  isBox :: SingleTile -> Bool
  isBox SBox = True
  isBox _    = False
  tileChar :: SingleTile -> Char
  tileChar SEmpty = '.'
  tileChar SWall  = '#'
  tileChar SBox   = 'O'
  tryMove :: TryMoveFun SingleTile
  tryMove = tryMoveNormal

solve1 :: PosMap SingleTile -> Point2 Int -> [Direction] -> Integer
solve1 = moveAndFindGPS

tryMoveNormal :: PosMap SingleTile -> Point2 Int -> Direction -> Maybe (PosMap SingleTile)
tryMoveNormal tileMap point direction = case Map.lookup point tileMap of
  Just SWall  -> Nothing
  Just SEmpty -> Just tileMap
  Just SBox   -> do
    tileMap' <- tryMoveNormal tileMap next direction
    return $ Map.insert point SEmpty $ Map.insert next SBox tileMap'
  Nothing    -> error "Should not happen"
  where
    next = moveByDir point direction

---------- Part 2 ---------

instance Tile WideTile where
  empty :: WideTile
  empty = WEmpty
  isBox :: WideTile -> Bool
  isBox WLeftBox = True
  isBox _        = False
  tileChar :: WideTile -> Char
  tileChar WEmpty    = '.'
  tileChar WWall     = '#'
  tileChar WLeftBox  = '['
  tileChar WRightBox = ']'
  tryMove :: TryMoveFun WideTile
  tryMove = tryMoveWide

solve2 :: PosMap WideTile -> Point2 Int -> [Direction] -> Integer
solve2 = moveAndFindGPS

tryMoveWide :: PosMap WideTile -> Point2 Int -> Direction -> Maybe (PosMap WideTile)
tryMoveWide tileMap point direction = case Map.lookup point tileMap of
  Just WWall     -> Nothing
  Just WEmpty    -> Just tileMap
  Just WLeftBox  -> if direction == East
    then do
      tileMap' <- tryMoveWide tileMap next2 direction
      return $ moveWideBox point direction tileMap'
    else do
      tileMap' <- tryMoveWide tileMap next direction
      tileMap'' <- tryMoveWide tileMap' nextRight direction
      return $ moveWideBox point direction tileMap''
  Just WRightBox -> if direction == West
    then do
      tileMap' <- tryMoveWide tileMap next2 direction
      return $ moveWideBox point direction tileMap'
    else do
      tileMap' <- tryMoveWide tileMap next direction
      tileMap'' <- tryMoveWide tileMap' nextLeft direction
      return $ moveWideBox point direction tileMap''
  Nothing        -> error "Should not happen"
  where
    next = moveByDir point direction
    next2 = moveByDir next direction
    nextLeft = moveByDir next West
    nextRight = moveByDir next East

moveWideBox :: Point2 Int -> Direction -> PosMap WideTile -> PosMap WideTile
moveWideBox point direction tileMap = case Map.lookup point tileMap of
  Just WLeftBox ->
    Map.insert next WLeftBox $
    Map.insert nextRight WRightBox $
    Map.insert point WEmpty $
    Map.insert rightPoint WEmpty tileMap
  Just WRightBox ->
    Map.insert next WRightBox $
    Map.insert nextLeft WLeftBox $
    Map.insert point WEmpty $
    Map.insert leftPoint WEmpty tileMap
  _ -> error "Invalid tile"
  where
    leftPoint = moveByDir point West
    rightPoint = moveByDir point East
    next = moveByDir point direction
    nextLeft = moveByDir next West
    nextRight = moveByDir next East

------ Common ------------

-- Try move should try to move the object (if any) in the specified position one step in the specified direction.
-- If the move is not possible, Nothing is returned.
-- Empty can always be moved, but it has no effect on the map
-- Wall can never be moved
-- Boxes can be moved only if the square(s) where the box would end up can be moved.
type TryMoveFun a = PosMap a -> Point2 Int -> Direction -> Maybe (PosMap a)

moveRobotAll :: Tile a => PosMap a -> Point2 Int -> [Direction] -> (PosMap a, Point2 Int)
moveRobotAll tileMap start = foldl (uncurry moveRobotSingle) (tileMap, start)

moveRobotSingle :: Tile a => PosMap a -> Point2 Int -> Direction -> (PosMap a, Point2 Int)
moveRobotSingle tileMap point direction = case tryMove tileMap next direction of
  Nothing       -> (tileMap, point)
  Just tileMap' -> (tileMap', next)
  where
    next = moveByDir point direction

moveAndFindGPS :: Tile a => PosMap a -> Point2 Int -> [Direction] -> Integer
moveAndFindGPS tileMap start instructions = sum $ map gps boxes
  where
    (tileMap', _pos) = moveRobotAll tileMap start instructions
    boxes = Map.keys $ Map.filter isBox tileMap'

gps :: Point2 Int -> Integer
gps (P2 x y) = 100 * toInteger y + toInteger x

showTileMap :: Tile a => PosMap a -> Point2 Int -> String
showTileMap tileMap pos = unlines $ map (map $ uncurry determineChar) matrix
  where
    matrix = groupBy (on (==) (\(P2 _ y, _) -> y)) $ sortOn (\(P2 x y, _) -> (y, x)) $ Map.toList tileMap
    determineChar p t
      | p == pos = '@'
      | otherwise = tileChar t

class Tile a where
  empty :: a
  isBox :: a -> Bool
  tileChar :: a -> Char
  tryMove :: TryMoveFun a

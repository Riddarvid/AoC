module Day10.Graphics (renderDay11Loop, renderDay11LoopScaled) where

import           AoCUtils.Geometry (Point2 (P2))
import qualified Data.HashMap.Lazy as HM
import           Data.HashSet      (HashSet)
import qualified Data.HashSet      as HS
import           Days.Day10        (Pos, Tile (..), exploreLoop, extendBorder,
                                    findInsidePoints, findOutsidePoints,
                                    loopTiles, parseInput, scaleMap)
import           Graphics.Gloss    (Display (InWindow), Picture, black, blank,
                                    circleSolid, color, display, green, line,
                                    pictures, rectangleSolid, red, scale,
                                    translate, white, yellow)

renderDay11Loop :: String -> IO ()
renderDay11Loop input = display (InWindow "Day 11 loop" (1000, 1000) (10, 10)) white loopDisp
  where
    loopDisp = loopDisplay input

renderDay11LoopScaled :: String -> IO ()
renderDay11LoopScaled input = display (InWindow "Day 11 loop" (1000, 1000) (10, 10)) white loopDisp
  where
    loopDisp = loopDisplayScaled input

loopDisplay :: String -> Picture
loopDisplay input = undefined
  --scale 2 2 $ pictures $ map (uncurry $ renderTile loopTiles' start) $ HM.toList tiles
  where
    (start, tiles, _, _) = parseInput input
    bfs = exploreLoop start tiles
    loopTiles' = loopTiles start bfs

loopDisplayScaled :: String -> Picture
loopDisplayScaled input = pictures $ map (uncurry $ renderTile loopTiles' start' inside) $ HM.toList tiles''
  where
    (P2 x y, tiles, maxX, maxY) = parseInput input
    start' = P2 (x * 2) (y * 2)
    tiles' = scaleMap tiles
    tiles'' = extendBorder tiles' (maxX * 2 + 1) (maxY * 2 + 1)
    bfs = exploreLoop start' tiles''
    loopTiles' = loopTiles start' bfs
    outside = findOutsidePoints tiles'' loopTiles'
    inside = findInsidePoints outside loopTiles' tiles''

renderTile :: HashSet Pos -> Pos -> HashSet Pos -> Pos -> Tile -> Picture
renderTile loopTiles' start inside pos@(P2 x y) tile =
    translate (fromIntegral x) (fromIntegral (-y)) pic
  where
    color'
      | HS.member pos inside = red
      | HS.member pos loopTiles' = green
      | otherwise = black
    pipe = renderPipe tile
    pic = if pos == start
      then color green $ rectangleSolid 1 1
      else color color' pipe

renderPipe :: Tile -> Picture
renderPipe tile = case tile of
  Empty -> circleSolid 0.2
  NS    -> line [(0, -0.5), (0, 0.5)]
  WE    -> line [(-0.5, 0), (0.5, 0)]
  NE    -> line [(0, 0.5), (0, 0), (0.5, 0)]
  NW    -> line [(0, 0.5), (0, 0), (-0.5, 0)]
  SW    -> line [(0, -0.5), (0, 0), (-0.5, 0)]
  SE    -> line [(0, -0.5), (0, 0), (0.5, 0)]
  _     -> blank

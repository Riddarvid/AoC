module Day10.Graphics (renderDay11Loop, renderDay11LoopScaled) where

import           AoCUtils.Geometry            (Point2 (P2))
import           Data.HashMap.Internal.Strict (HashMap)
import qualified Data.HashMap.Lazy            as HM
import           Data.HashSet                 (HashSet)
import qualified Data.HashSet                 as HS
import           Days.Day10                   (Pos, Tile (..), exploreLoop,
                                               findInsideScaled, loopTiles,
                                               parseInput, scaleUpMap,
                                               scaleUpStart)
import           Graphics.Gloss               (Display (InWindow), Picture,
                                               blank, blue, circleSolid, color,
                                               display, greyN, line, pictures,
                                               rectangleSolid, red, translate,
                                               white)

renderDay11Loop :: String -> IO ()
renderDay11Loop input = display (InWindow "Day 11 loop" (1000, 1000) (10, 10)) white loopDisp
  where
    loopDisp = loopDisplay input

renderDay11LoopScaled :: String -> IO ()
renderDay11LoopScaled input = display (InWindow "Day 11 loop" (1000, 1000) (10, 10)) white loopDisp
  where
    loopDisp = loopDisplayScaled input

loopDisplay :: String -> Picture
loopDisplay input =
  pictures [
    color (greyN 0.9) $ renderMap tiles,
    color red $ renderSet tiles loop,
    color blue $ renderStart start
  ]
  where
    (start, tiles, _, _) = parseInput input
    bfs = exploreLoop start tiles
    loop = loopTiles start bfs

loopDisplayScaled :: String -> Picture
loopDisplayScaled input =
  pictures [
    color (greyN 0.9) $ renderMap tiles',
    color blue $ renderSet tiles' inside,
    color red $ renderSet tiles' loop,
    color blue $ renderStart start'
  ]
  where
    (start, tiles, maxX, maxY) = parseInput input
    start' = scaleUpStart start
    tiles' = scaleUpMap tiles
    loop = loopTiles start' $ exploreLoop start' tiles'
    inside = findInsideScaled start tiles maxX maxY

renderMap :: HashMap Pos Tile -> Picture
renderMap tiles =
  pictures $ map (uncurry renderTile) $ HM.toList tiles

renderSet :: HashMap Pos Tile -> HashSet Pos -> Picture
renderSet tiles set = renderMap $ HM.filterWithKey (\pos _ -> pos `HS.member` set) tiles

renderStart :: Pos -> Picture
renderStart pos = translatePos pos $ rectangleSolid 1 1

renderTile :: Pos -> Tile -> Picture
renderTile pos tile = translatePos pos tilePicture
  where
    tilePicture = case tile of
      Empty -> circleSolid 0.2
      NS    -> line [(0, -0.5), (0, 0.5)]
      WE    -> line [(-0.5, 0), (0.5, 0)]
      NE    -> line [(0, 0.5), (0, 0), (0.5, 0)]
      NW    -> line [(0, 0.5), (0, 0), (-0.5, 0)]
      SW    -> line [(0, -0.5), (0, 0), (-0.5, 0)]
      SE    -> line [(0, -0.5), (0, 0), (0.5, 0)]
      _     -> blank

translatePos :: Pos -> Picture -> Picture
translatePos (P2 x y) = translate (fromIntegral x) (fromIntegral (-y))

{-# LANGUAGE BangPatterns #-}
module Days.Day9.Graphics (drawTiles) where
import           AoCUtils.Geometry (Point2 (P2))
import           AoCUtils.Graphics (square)
import           Days.Day9         (findLargestRectangle, parsePoint)
import qualified Days.Day9         as D9
import           Graphics.Gloss    (Display (InWindow), Picture, blue, color,
                                    display, green, pictures, rectangleSolid,
                                    rectangleWire, red, translate, white)

drawTiles :: String -> IO ()
drawTiles input = do
  let basePoints = map parsePoint $ lines input :: [Point2 Integer]
  let !baseSolution = findLargestRectangle basePoints
  let disp = drawDisplay basePoints baseSolution
  display (InWindow "Day 9 tiles" (1000, 1000) (10, 10)) white disp

drawDisplay :: Real a => [Point2 a] -> D9.Rectangle a -> Picture
drawDisplay basePoints baseSolution = translate (- realToFrac baseX) (- realToFrac baseY) pic
  where
    scale' = 100
    points = map (\(P2 x y) -> P2 (realToFrac x / scale') (realToFrac y / scale')) basePoints
    P2 baseX baseY = head points :: Point2 Double
    drawnCorners = drawCorners points
    drawnLines = drawLines points
    drawnSolution = drawSolution $ scaleSolution scale' baseSolution
    pic = drawnCorners <> drawnLines <> drawnSolution

scaleSolution :: (Real a, Fractional b) => b -> D9.Rectangle a -> D9.Rectangle b
scaleSolution scale' (p1, p2) = (scalePoint scale' p1, scalePoint scale' p2)

scalePoint :: (Real a, Fractional b) => b -> Point2 a -> Point2 b
scalePoint scale' (P2 x y) = P2 (realToFrac x / scale') (realToFrac y / scale')

drawSolution :: Real a => D9.Rectangle a -> Picture
drawSolution (P2 x1 y1, P2 x2 y2) = translate tx ty $ color green $ rectangleWire width height
  where
    width = realToFrac $ abs (x1 - x2)
    height = realToFrac $ abs (y1 - y2)
    tx = realToFrac (x1 + x2) / 2
    ty = realToFrac (y1 + y2) / 2

drawCorners :: Real a => [Point2 a] -> Picture
drawCorners = pictures . map drawCorner

drawCorner :: Real a => Point2 a -> Picture
drawCorner p = translateToPoint p $ color red square

drawLines :: Real a => [Point2 a] -> Picture
drawLines points = pictures $ map (uncurry drawLine) $ mkPairs points'
  where
    points' = points ++ [head points]

drawLine :: Real a => Point2 a -> Point2 a -> Picture
drawLine (P2 x1 y1) (P2 x2 y2) = translate tx ty $ color blue $ rectangleSolid width height
  where
    width = realToFrac $ if x1 == x2 then 1 else abs (x2 - x1) - 1
    height = realToFrac $ if y1 == y2 then 1 else abs (y2 - y1) - 1
    tx = realToFrac (x1 + x2) / 2
    ty = realToFrac (y1 + y2) / 2

mkPairs :: [a] -> [(a, a)]
mkPairs (x : y : zs) = (x, y) : mkPairs (y : zs)
mkPairs _            = []

translateToPoint :: (Real a) => Point2 a -> Picture -> Picture
translateToPoint (P2 x y) = translate x' y'
  where
    x' = realToFrac x
    y' = realToFrac y

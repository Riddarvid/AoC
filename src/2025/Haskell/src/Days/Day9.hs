module Days.Day9 (solve) where
import           AoCUtils.Days     (Solver)
import           AoCUtils.Geometry (Point2 (P2, p2X, p2Y))
import           Data.Foldable     (find)
import           Data.Maybe        (fromJust)
import           Debug.Trace       (trace, traceShow)
import           Utils             (mkUniquePairs, split)

type Corner = (Point2 Integer, Point2 Integer, Point2 Integer)

type Rectangle = (Corner, Corner)

data Orientation = OLeft | ORight
  deriving Show

data Direction = DEast | DSouth | DWest | DNorth

solve :: Solver
solve input = let
  points = map parsePoint $ lines input
  corners = mkCorners points
  part1 = solve1 corners
  part2 = solve2 corners
  in (show part1, show part2)

parsePoint :: String -> Point2 Integer
parsePoint input = case split ',' input of
  [xString, yString] -> P2 (read xString) (read yString)
  _                  -> error "Could not parse point"

solve1 :: [Corner] -> Integer
solve1 corners = maximum $ map area rectangles
  where
    rectangles = mkUniquePairs corners

cornerPoint :: Corner -> Point2 Integer
cornerPoint (_, p, _) = p

area :: Rectangle -> Integer
area (c1, c2) = area' (cornerPoint c1) (cornerPoint c2)

area' :: Point2 Integer -> Point2 Integer -> Integer
area' (P2 x1 y1) (P2 x2 y2) = dx * dy
  where
    dx = abs (x1 - x2) + 1
    dy = abs (y1 - y2) + 1

solve2 :: [Corner] -> Integer
solve2 corners = maximum $ map area insideRects
  where
    insideOrientation = findInsideOrientation corners
    rectangles = mkUniquePairs corners
    nonIntersected = filter (\rect -> not $ any (rect `isIntersectedBy`) corners) rectangles
    insideRects = filter (isInside insideOrientation) nonIntersected

findInsideOrientation :: [Corner] -> Orientation
findInsideOrientation corners = case findDir p1 p2 of
  DSouth -> OLeft
  DWest  -> ORight
  _      -> error "Should not happen"
  where
    minX = minimum $ map (p2X . cornerPoint) corners
    minYforMinX = minimum $
      map (p2Y . cornerPoint) $
      filter (\(_, p, _) -> p2X p == minX) corners
    targetPoint = P2 minX minYforMinX
    targetCorner = fromJust $ find (\c -> cornerPoint c == targetPoint) corners
    (p1, p2, _) = targetCorner


isInside :: Orientation -> Rectangle -> Bool
isInside insideOrientation (c1, c2) = testPoint `elem` insidePoints
  where
    P2 x1 y1 = cornerPoint c1
    P2 x2 y2 = cornerPoint c2
    testPoint = P2 x y
    x = if x1 < x2 then x1 + 1 else x1 - 1
    y = if y1 < y2 then y1 + 1 else y1 - 1
    insidePoints = findInsidePoint c1 insideOrientation

findInsidePoint :: Corner -> Orientation -> [Point2 Integer]
findInsidePoint (p1, p2, p3) orientation = case orientation of
  OLeft  -> lefts
  ORight -> rights
  where
    dir1 = findDir p1 p2
    dir2 = findDir p2 p3
    P2 x y = p2
    pnw = P2 (x - 1) (y + 1)
    pne = P2 (x + 1) (y + 1)
    psw = P2 (x - 1) (y - 1)
    pse = P2 (x + 1) (y - 1)
    (lefts, rights) = case dir1 of
      DEast -> case dir2 of
        DNorth -> ([pnw], [pne, psw, pse])
        DSouth -> ([pnw, pne, pse], [psw])
        _      -> error "Not a corner"
      DSouth -> case dir2 of
        DEast -> ([pne], [pnw, psw, pse])
        DWest -> ([pne, psw, pse], [pnw])
        _     -> error "Not a corner"
      DWest -> case dir2 of
        DNorth -> ([pnw, psw, pse], [pne])
        DSouth -> ([pse], [pnw, pnw, psw])
        _      -> error "Not a corner"
      DNorth -> case dir2 of
        DEast -> ([pnw, pne, psw], [pse])
        DWest -> ([psw], [pnw, pne, pse])
        _     -> error "Not a corner"

findDir :: Point2 Integer -> Point2 Integer -> Direction
findDir (P2 x1 y1) (P2 x2 y2)
  | x1 == x2 = if y1 < y2 then DNorth else DSouth
  | y1 == y2 = if x1 < x2 then DEast else DWest
  | otherwise = error "Points are not on a line"

isIntersectedBy :: Rectangle -> Corner -> Bool
isIntersectedBy rect (p1, cp, p3) = any (contains rect) [cp1, cp, cp2]
  where
    P2 x y = cp
    cp1 = case findDir p1 cp of
      DEast  -> P2 (x - 1) y
      DSouth -> P2 x (y + 1)
      DWest  -> P2 (x + 1) y
      DNorth -> P2 x (y - 1)
    cp2 = case findDir cp p3 of
      DEast  -> P2 (x + 1) y
      DSouth -> P2 x (y - 1)
      DWest  -> P2 (x - 1) y
      DNorth -> P2 x (y + 1)

contains :: Rectangle -> Point2 Integer -> Bool
contains ((_, P2 x1 y1, _), (_, P2 x2 y2, _)) (P2 x y) =
  contains1D (x1, x2) x &&
  contains1D (y1, y2) y

contains1D :: (Integer, Integer) -> Integer -> Bool
contains1D (x1, x2) x
  | x1 < x2 = x1 < x && x < x2
  | otherwise = x2 < x && x < x1

mkCorners :: [Point2 Integer] -> [Corner]
mkCorners (x : y : zs) = mkCorners' (x : y: zs ++ [x, y])
mkCorners _            = []

mkCorners' :: [Point2 Integer] -> [Corner]
mkCorners' (x : y : z : zs) = (x, y, z) : mkCorners' (y : z : zs)
mkCorners' _                = []

showRect :: Rectangle -> String
showRect (c1, c2) = show (cornerPoint c1, cornerPoint c2)

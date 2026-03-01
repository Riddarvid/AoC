module Days.Day9 (
  solve,
  parsePoint,
  findLargestRectangle,
  Rectangle
) where
import           AoCUtils.Days     (Solver)
import           AoCUtils.Geometry (Point2 (P2))
import           Data.Foldable     (maximumBy)
import           Data.Ord          (comparing)
import           Utils             (mkUniquePairs, split)

type Rectangle a = (Point2 a, Point2 a)

-- Lines should be constructed so that the first point is smaller
-- than the second.
data Line a = HLine a (a, a) | VLine a (a, a)

solve :: Solver
solve input = let
  points = map parsePoint $ lines input :: [Point2 Integer]
  part1 = solve1 points
  part2 = solve2 points
  in (show part1, show part2)

parsePoint :: Read a => String -> Point2 a
parsePoint input = case split ',' input of
  [xString, yString] -> P2 (read xString) (read yString)
  _                  -> error "Could not parse point"

solve1 :: (Ord a, Num a) => [Point2 a] -> a
solve1 points = maximum $ map (uncurry area) rectangles
  where
    rectangles = mkUniquePairs points

area :: Num a => Point2 a -> Point2 a -> a
area (P2 x1 y1) (P2 x2 y2) = dx * dy
  where
    dx = abs (x1 - x2) + 1
    dy = abs (y1 - y2) + 1

solve2 :: (Num a, Ord a) => [Point2 a] -> a
solve2 = uncurry area . findLargestRectangle

-- By looking at the data, it seems like the largest recatngle will never be on the
-- outside. So we only check for intersection.
findLargestRectangle :: (Num a, Ord a) => [Point2 a] -> Rectangle a
findLargestRectangle points = maximumBy (comparing $ uncurry area) nonIntersected
  where
    allRectangles = mkUniquePairs points
    lines' = mkLines points
    nonIntersected = filter (\rect -> not $ any (`intersects` rect) lines') allRectangles

intersects :: Ord a => Line a -> Rectangle a -> Bool
intersects line rect = case line of
  HLine y xLine ->
    (y `liesWithin` (rectMinY, rectMaxY)) &&
    intersects1D xLine (rectMinX, rectMaxX)
  VLine x yLine ->
    (x `liesWithin` (rectMinX, rectMaxX)) &&
    intersects1D yLine (rectMinY, rectMaxY)
  where
    (rectMinX, rectMinY, rectMaxX, rectMaxY) = rectDimensions rect

rectDimensions :: Ord a => Rectangle a -> (a, a, a, a)
rectDimensions (P2 x1 y1, P2 x2 y2) = (minX, minY, maxX, maxY)
  where
    (minX, maxX) = orderPair x1 x2
    (minY, maxY) = orderPair y1 y2

intersects1D :: Ord a => (a, a) -> (a, a) -> Bool
intersects1D (min1, max1) (min2, max2) =
  max1 > min2 &&
  min1 < max2

liesWithin :: Ord a => a -> (a, a) -> Bool
liesWithin x (minX, maxX) = minX < x && x < maxX

mkLines :: (Ord a) =>[Point2 a] -> [Line a]
mkLines points = map (uncurry mkLine) $ mkPairs (points ++ [head points])

mkPairs :: [a] -> [(a, a)]
mkPairs (x : y : zs) = (x, y) : mkPairs (y : zs)
mkPairs _            = []

mkLine :: (Ord a) =>Point2 a -> Point2 a -> Line a
mkLine (P2 x1 y1) (P2 x2 y2)
  | x1 == x2 = VLine x1 (orderPair y1 y2)
  | y1 == y2 = HLine y1 (orderPair x1 x2)
  | otherwise = error "Not a line"

orderPair :: Ord a => a -> a -> (a, a)
orderPair x y
  | x < y = (x, y)
  | otherwise = (y, x)

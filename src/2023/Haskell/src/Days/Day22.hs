{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs  #-}
module Days.Day22 (
  solve,
  propNoCollisionsLanded,
  propRemoveCollisions,
  propLandedSupported
) where
import           AoCUtils.Days     (Solver)
import           AoCUtils.Geometry (Point (moveBy), Point3 (P3), Vector3)
import           AoCUtils.Regex    (parseSignedInts)
import           Data.Hashable     (Hashable)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import           Data.HashSet      (HashSet)
import qualified Data.HashSet      as HS
import           Data.Ix           (Ix (inRange))
import           Data.List         (nub, partition)
import           Data.Maybe        (fromJust)
import           GHC.Generics      (Generic)
import           Test.QuickCheck   (Arbitrary (arbitrary), Gen, Property,
                                    Testable (property), chooseInt, elements)


-- A naive solution is of course to simply check for each point if it is contained within the
-- other brick. But it's more fun to find a more efficient method.
-- We have two scenarios:
-- 1) The bricks extend in the same direction. FOr example, if both bricks extend
-- in the z-direction, then they would have to have the esact same (x, y) coordinates in order
-- to be able to intersect. Also, the z-ranges would have to intersect.
-- 2) The bricks extend in different directions. In this case, the bricks can intersect in
-- at most one point. For example, if one brick extends in the x-direction and one extends
-- in the y-direction. Then they would need to have the same z coordinate in order to be
-- able to intersect. Also, the x-range of the first needs to contain the y-coordinate
-- of the second, and the y-range of the second needs to contain the x-coordinate
-- of the first.
-- This should describe exactly the process needed to determine if two lines intersect
-- each other.
-- Support, on the other hand should be able to be determined via intersection with a line
-- simply moved up a single step. If our stacking process is correct, there should be
-- no intersections to start with, so we don't need to consider this case.

type Pos = Point3 Int

-- The higher of the varying coordinate should always be in the second entry.
data Brick = Brick Pos Pos
  deriving (Eq, Generic)

instance Hashable Brick

data Orientation = X | Y | Z | Any
  deriving (Eq, Show)

solve :: Solver
solve input = let
  bricks = map parseBrick $ lines input
  landed = stackBricks bricks
  supporting = mapSupporting landed
  supportedBy = mapSupportedBy landed
  part1 = solve1 supportedBy
  part2 = solve2 supporting supportedBy
  in (show part1, show part2)

-- Parsing

parseBrick :: String -> Brick
parseBrick line = uncurry Brick $ orderPoints p1 p2
  where
    vals = parseSignedInts line
    p1 = P3 (vals !! 0) (vals !! 1) (vals !! 2)
    p2 = P3 (vals !! 3) (vals !! 4) (vals !! 5)

comparePoints :: Pos -> Pos -> Ordering
comparePoints (P3 x1 y1 z1) (P3 x2 y2 z2) = case compare x1 x2 of
  EQ -> case compare y1 y2 of
    EQ  -> compare z1 z2
    res -> res
  res -> res

orderPoints :: Pos -> Pos -> (Pos, Pos)
orderPoints p1 p2 = case comparePoints p1 p2 of
  LT -> (p1, p2)
  GT -> (p2, p1)
  EQ -> (p1, p2)

-- Part 1

solve1 :: HashMap Brick [Brick] -> Int
solve1 supportedBy = HS.size safe
  where
    essential = HS.fromList $ concat $ HM.elems $ HM.filter (\bs -> length bs == 1) supportedBy
    safe = HS.difference (HM.keysSet supportedBy) essential

stackBricks :: [Brick] -> [Brick]
stackBricks = stackBricks' []

stackBricks' :: [Brick] -> [Brick] -> [Brick]
stackBricks' landed [] = landed
stackBricks' landed falling = stackBricks' landed' falling''
  where
    (landed', falling') = detectLanded falling landed
    falling'' = map moveDownOneStep falling'

-- Moving bricks

moveUpOneStep :: Brick -> Brick
moveUpOneStep = moveBrickBy (P3 0 0 1)

moveDownOneStep :: Brick -> Brick
moveDownOneStep = moveBrickBy (P3 0 0 (-1))

moveBrickBy :: Vector3 Int -> Brick -> Brick
moveBrickBy v (Brick p1 p2) = Brick (p1 `moveBy` v) (p2 `moveBy` v)

-- Intersections

-- Call recursively if you find at least one new
detectLanded :: [Brick] -> [Brick] -> ([Brick], [Brick])
detectLanded falling landed = case landed' of
  [] -> (landed, falling)
  _  -> detectLanded falling' (landed' ++ landed)
  where
    (landed', falling') =
      partition (\f -> isOnGround f || any (\l -> f `isSupportedBy` l) landed) falling

isOnGround :: Brick -> Bool
isOnGround (Brick (P3 _ _ z) _) = z == 1

isSupportedBy :: Brick -> Brick -> Bool
isSupportedBy b1 b2
  | b1 == b2 = False
  | otherwise = intersects b1 (moveUpOneStep b2)

intersects :: Brick -> Brick -> Bool
intersects b1 b2
  | o1 == Any = intersectsPoint b2 b1
  | o2 == Any = intersectsPoint b1 b2
  | o1 == o2 = intersectsParallel b1 b2
  | otherwise = intersectsOrthogonal b1 b2
  where
    o1 = orientationOf b1
    o2 = orientationOf b2

-- The second brick must be a single point
intersectsPoint :: Brick -> Brick -> Bool
intersectsPoint brick pointBrick = case orientation of
  Any -> brick == pointBrick
  _   -> orthCoords1 == orthCoords2 && min' <= pointCoord && pointCoord <= max'
  where
    orientation = orientationOf brick
    (orth1, orth2) = orthogonalPlane orientation
    orthCoords1 = (getCoord orth1 brick, getCoord orth2 brick)
    orthCoords2 = (getCoord orth1 pointBrick, getCoord orth2 pointBrick)
    (min', max') = getRange orientation brick
    pointCoord = getCoord orientation pointBrick

-- We know that no brick has orientation Any.
intersectsParallel :: Brick -> Brick -> Bool
intersectsParallel b1 b2 = orthCoords1 == orthCoords2 && intersectsRange paraRange1 paraRange2
  where
    orientation = orientationOf b1
    (orth1, orth2) = orthogonalPlane orientation
    orthCoords1 = (getCoord orth1 b1, getCoord orth2 b1)
    orthCoords2 = (getCoord orth1 b2, getCoord orth2 b2)
    paraRange1 = getRange orientation b1
    paraRange2 = getRange orientation b2

intersectsOrthogonal :: Brick -> Brick -> Bool
intersectsOrthogonal brick1 brick2 =
  getCoord orthOrientation brick1 == getCoord orthOrientation brick2 &&
    inRange range1 coord1 && inRange range2 coord2
  where
    o1 = orientationOf brick1
    o2 = orientationOf brick2
    orthOrientation = orthogonalLine o1 o2
    coord1 = getCoord o1 brick2
    range1 = getRange o1 brick1
    coord2 = getCoord o2 brick1
    range2 = getRange o2 brick2

getRange :: Orientation -> Brick -> (Int, Int)
getRange orientation (Brick p1 p2) = (getCoord'' p1, getCoord'' p2)
  where
    getCoord'' = getCoord' orientation

getCoord :: Orientation -> Brick -> Int
getCoord orientation (Brick p _) = getCoord' orientation p

getCoord' :: Orientation -> Pos -> Int
getCoord' orientation (P3 x y z) = case orientation of
  X   -> x
  Y   -> y
  Z   -> z
  Any -> undefined

orthogonalPlane :: Orientation -> (Orientation, Orientation)
orthogonalPlane X   = (Y, Z)
orthogonalPlane Y   = (X, Z)
orthogonalPlane Z   = (X, Y)
orthogonalPlane Any = undefined

orthogonalLine :: Orientation -> Orientation -> Orientation
orthogonalLine X Y = Z
orthogonalLine Y X = Z
orthogonalLine X Z = Y
orthogonalLine Z X = Y
orthogonalLine Y Z = X
orthogonalLine Z Y = X
orthogonalLine _ _ = undefined

orientationOf :: Brick -> Orientation
orientationOf (Brick (P3 x1 y1 z1) (P3 x2 y2 z2))
  | x1 /= x2 = X
  | y1 /= y2 = Y
  | z1 /= z2 = Z
  | otherwise = Any

intersectsRange :: (Int, Int) -> (Int, Int) -> Bool
intersectsRange (min1, max1) (min2, max2) =
  (min1 <= max2) &&
  (min2 <= max1)

instance Arbitrary Brick where
  arbitrary :: Gen Brick
  arbitrary = do
    p1 <- choosePoint
    p2 <- chooseComplementPoint p1
    return $ Brick p1 p2

choosePoint :: Gen Pos
choosePoint = do
  x <- chooseXY 0
  y <- chooseXY 0
  z <- chooseZ 1
  return $ P3 x y z

chooseComplementPoint :: Pos -> Gen Pos
chooseComplementPoint (P3 x y z) = do
  orientation <- chooseOrientation
  case orientation of
    X -> do
      x' <- chooseXY x
      return $ P3 x' y z
    Y -> do
      y' <- chooseXY y
      return $ P3 x y' z
    Z -> do
      z' <- chooseZ z
      return $ P3 x y z'
    Any -> error "Should not be able to generate an Any value"

chooseOrientation :: Gen Orientation
chooseOrientation = elements [X, Y, Z]

chooseXY :: Int -> Gen Int
chooseXY n = chooseInt (n, 9)

chooseZ :: Int -> Gen Int
chooseZ n = chooseInt (n, n + 10)

propRemoveCollisions :: [Brick] -> Property
propRemoveCollisions = property . not . hasCollisions . removeCollisions

propNoCollisionsLanded :: [Brick] -> Property
propNoCollisionsLanded bricks = property $ not $ hasCollisions landed
  where
    bricks' = removeCollisions bricks
    landed = stackBricks bricks'

propLandedSupported :: [Brick] -> Property
propLandedSupported bricks = property $ all (\l -> isOnGround l || any (l `isSupportedBy`) landed) landed
  where
    landed = stackBricks $ removeCollisions bricks

hasCollisions :: [Brick] -> Bool
hasCollisions bricks = any (\b -> any (\b' -> b /= b' && intersects b b') bricks) bricks

removeCollisions :: [Brick] -> [Brick]
removeCollisions = foldr tryAddBrick []

tryAddBrick :: Brick -> [Brick] -> [Brick]
tryAddBrick brick bricks
  | any (intersects brick) bricks = bricks
  | otherwise = brick : bricks

solve2 :: HashMap Brick [Brick] -> HashMap Brick [Brick] -> Int
solve2 supporting supportedBy =
  sum $ map (nWouldFall supporting supportedBy) $ HM.keys supportedBy

mapSupporting :: [Brick] -> HashMap Brick [Brick]
mapSupporting landed = HM.fromList $ map (\l -> (l, filter (`isSupportedBy` l) landed)) landed

mapSupportedBy :: [Brick] -> HashMap Brick [Brick]
mapSupportedBy landed = HM.fromList $ map (\l -> (l, filter (isSupportedBy l) landed)) landed

nWouldFall :: HashMap Brick [Brick] -> HashMap Brick [Brick] -> Brick -> Int
nWouldFall supporting supported brick =
  HS.size (wouldFallTotal (HS.singleton brick) [brick] supporting supported) - 1

wouldFallTotal :: HashSet Brick -> [Brick] ->
  HashMap Brick [Brick] -> HashMap Brick [Brick] -> HashSet Brick
wouldFallTotal removed [] _ _ = removed
wouldFallTotal removed removedLast supporting supportedBy =
  wouldFallTotal (foldr HS.insert removed wouldFall') wouldFall' supporting supportedBy
  where
    candidates = nub $ concatMap (fromJust . (`HM.lookup` supporting)) removedLast
    wouldFall' = filter (wouldFall supportedBy removed) candidates

wouldFall :: HashMap Brick [Brick] -> HashSet Brick -> Brick -> Bool
wouldFall _ _ brick | isOnGround brick = False
wouldFall supported removed brick = all (`HS.member` removed) supporters
  where
    supporters = fromJust $ HM.lookup brick supported

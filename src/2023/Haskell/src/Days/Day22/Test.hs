module Days.Day22.Test (
  propRemoveCollisions,
  propNoCollisionsLanded,
  propLandedSupported
) where
import           AoCUtils.Geometry (Point3 (P3))
import           Days.Day22        (Brick (Brick), Orientation (..), Pos,
                                    intersects, isOnGround, isSupportedBy,
                                    stackBricks)
import           Test.QuickCheck   (Gen, Property, Testable (property),
                                    chooseInt, elements, forAll, listOf)

propRemoveCollisions :: Property
propRemoveCollisions = forBricks (property . not . hasCollisions . removeCollisions)

propNoCollisionsLanded :: Property
propNoCollisionsLanded = forBricks
  (property . not . hasCollisions . stackBricks . removeCollisions)

propLandedSupported :: Property
propLandedSupported = forBricks
  (\bricks -> let landed = stackBricks $ removeCollisions bricks in
    property $ all (\l -> isOnGround l || any (l `isSupportedBy`) landed) landed)

hasCollisions :: [Brick] -> Bool
hasCollisions bricks = any (\b -> any (\b' -> b /= b' && intersects b b') bricks) bricks

removeCollisions :: [Brick] -> [Brick]
removeCollisions = foldr tryAddBrick []

tryAddBrick :: Brick -> [Brick] -> [Brick]
tryAddBrick brick bricks
  | any (intersects brick) bricks = bricks
  | otherwise = brick : bricks

forBricks :: ([Brick] -> Property) -> Property
forBricks = forAll (listOf generateBrick)

-- Generators

generateBrick :: Gen Brick
generateBrick = do
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

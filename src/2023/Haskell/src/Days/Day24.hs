{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE TupleSections #-}
module Days.Day24 (solve, test, Hail(..), parseInput) where
import           AoCUtils.Days  (Solver)
import           AoCUtils.Regex (parseSignedInts)
import           Control.Spoon  (spoon)
import           Data.Maybe     (mapMaybe)
import           Data.Ratio     (denominator, numerator)
import           Linear         (V2 (V2), V3 (V3), crossZ, luSolveFinite,
                                 negated, transpose, (*^), (^+^), (^-^))

data Hail = Hail (V3 Integer) (V3 Integer)
  deriving (Eq)

instance Show Hail where
  show :: Hail -> String
  show (Hail (V3 px py pz) (V3 vx vy vz)) =
    show px ++ ", " ++ show py ++ ", " ++ show pz ++ " @ " ++
    show vx ++ ", " ++ show vy ++ ", " ++ show vz

solve :: Solver
solve input = let
  hails = map parseHail $ lines input
  part1 = solve1 hails
  part2 = solve2 hails
  in (show part1, show part2)

parseInput :: String -> [Hail]
parseInput = map parseHail . lines

parseHail :: String -> Hail
parseHail line = Hail
  (V3 (ints !! 0) (ints !! 1) (ints !! 2))
  (V3 (ints !! 3) (ints !! 4) (ints !! 5))
  where
    ints = parseSignedInts line

-- Doesn't catch parallel lines, must check separately.
test :: V2 Double
test = luSolveFinite a b
  where
    a = V2 (V2 3 3) (V2 2 2)
    b = V2 1 1

solve1 :: [Hail] -> Int
solve1 hails = length $ filter (uncurry intersects2D) pairs
  where
    pairs = mkPairs hails

mkPairs :: [a] -> [(a, a)]
mkPairs []       = []
mkPairs (x : xs) = map (x,) xs ++ mkPairs xs

-- We assume that no hails are on the exact same trajectory
intersects2D :: Hail -> Hail -> Bool
intersects2D h1 h2
  | areParallel v1 v2 = False
  | t < 0 || u < 0 = False -- Collided in the past
  | x < min' || max' < x || y < min' || y > max' = False -- Collide outside test area
  | otherwise = True
  where
    (p1, v1) = extractDs extractXY h1
    (p2, v2) = extractDs extractXY h2
    a = transpose $ V2 v1 (negated v2)
    b = p2 ^-^ p1
    (V2 t u) = luSolveFinite (fmap (fmap toRational) a) (fmap toRational b)
    (V2 x y) = fmap toRational p1 ^+^ (t *^ fmap toRational v1)
    min' = 200000000000000
    max' = 400000000000000

areParallel :: V2 Integer -> V2 Integer -> Bool
areParallel v1 v2 = crossZ v1 v2 == 0

extractDs :: (V3 Integer -> V2 Integer) -> Hail -> VectorPair
extractDs ef (Hail pos vel) = (ef pos, ef vel)

extractXY :: V3 a -> V2 a
extractXY (V3 x y _) = V2 x y

extractYZ :: V3 a -> V2 a
extractYZ (V3 _ y z) = V2 y z

type VectorPair = (V2 Integer, V2 Integer)

solve2 :: [Hail] -> Integer
solve2 hails = x + y + z
  where
    (Hail (V3 x y z) _) = findColliding hails

collidesWithAll :: [VectorPair] -> VectorPair -> Bool
collidesWithAll hails hail = all (collidesWith hail) hails

-- TODO: Try solving in one dimension at a time using the Chinese remainder theorem
-- or some variation thereof. Iterative solutions to systems of two diophantine equations?
collidesWith :: VectorPair -> VectorPair -> Bool
collidesWith (p1, v1) (p2, v2) =
  and $ collidesCoord <$> (p2 ^-^ p1) <*> (v1 ^-^ v2)
  where
    collidesCoord :: Integer -> Integer -> Bool
    collidesCoord dp dv
      | dv == 0 = dp == 0
      | otherwise = dp `mod` dv == 0 && dp `div` dv > 0

findColliding :: [Hail] -> Hail
findColliding hails = Hail (V3 px py pz) (V3 vx vy vz)
  where
    (V2 px py, V2 vx vy) = findColliding' hails $ extractDs extractXY
    (V2 _ pz, V2 _ vz) = findColliding' hails $ extractDs extractYZ

findColliding' :: [Hail] -> (Hail -> VectorPair) -> VectorPair
findColliding' hails ef = head $ filter (collidesWithAll hails2D) $ findCollidingVP h1 h2
  where
    hails2D = map ef hails
    h1 = hails2D !! 0
    h2 = hails2D !! 1

findCollidingVP :: VectorPair -> VectorPair -> [VectorPair]
findCollidingVP h1 h2 = mapMaybe (collidingVel h1 h2) velocities
  where
    velocities = [V2 x y | x <- coordRange, y <- coordRange]
    coordRange = [-1000 .. 1000]

collidingVel :: VectorPair -> VectorPair -> V2 Integer -> Maybe VectorPair
collidingVel (p1, v1) (p2, v2) v = do
  ts@(V2 t1 t2) <- unsafeLuSolveFinite a b
  t1' <- rationalToInteger t1
  _ <- rationalToInteger t2
  boolToMaybe $ all (> 0) ts
  let p = p1 ^+^ (t1' *^ (v1 ^-^ v))
  return (p, v)
  where
    t1c = v1 ^-^ v
    t2c = v ^-^ v2
    a = fmap toRational <$> transpose (V2 t1c t2c)
    b = toRational <$> (p2 ^-^ p1)

rationalToInteger :: Rational -> Maybe Integer
rationalToInteger n = if denominator n == 1 then Just (numerator n) else Nothing

boolToMaybe :: Bool -> Maybe ()
boolToMaybe False = Nothing
boolToMaybe True  = Just ()

-- luSolveFinite is partial so to handle it in pure code we have to do this ugly hack.
-- Spoon calls performUnsafeIO in the background.
unsafeLuSolveFinite :: V2 (V2 Rational) -> V2 Rational -> Maybe (V2 Rational)
unsafeLuSolveFinite a b = spoon $ luSolveFinite a b

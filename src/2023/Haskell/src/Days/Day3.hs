module Days.Day3 (solve) where
import           AoCUtils.Days     (Solver)
import           AoCUtils.Geometry (Point (moveBy), Point2 (P2), leftV)
import           AoCUtils.Matrices (matrixToHashMap)
import           Data.Char         (isDigit)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import           Data.Maybe        (catMaybes, isJust)

type P = Point2 Int

type IntegerMap = HashMap P Integer
type CharMap = HashMap P Char

solve :: Solver
solve input = let
  (digits, symbols) = parseInput input
  numbers = parseNumbers digits
  part1 = solve1 numbers symbols
  part2 = solve2 digits numbers symbols
  in (show part1, show part2)

-- We will make a map where the first position of each number maps to the number.
-- Then, we can use the length of the number to figure out which positions it occupies.
-- So the algorithm becomes
-- 1) For each number, find all its positions.
-- 2) For each of those, check if they are adjacent to a symbol
-- 3) If they are, add them to the list of part numbers

parseInput :: String -> (CharMap, CharMap)
parseInput input = let
  (charMap, _, _) = matrixToHashMap $ lines input
  digits = HM.filter isDigit charMap
  symbols = HM.filter (\c -> not (isDigit c) && (c /= '.')) charMap
  in (digits, symbols)

parseNumbers :: CharMap -> IntegerMap
parseNumbers digits = HM.mapMaybeWithKey (readNumber digits) digits

-- Reads a number only if this is the first character of the number
readNumber :: CharMap -> P -> Char -> Maybe Integer
readNumber digits p _ = case HM.lookup left digits of
  Nothing -> Just $ readNumber' digits p
  Just _  -> Nothing
  where
    left = p `moveBy` leftV

-- Reads a number starting at p
readNumber' :: CharMap -> P -> Integer
readNumber' digits (P2 startX startY) = read digits'
  where
    digits' = catMaybes $ takeWhile isJust [HM.lookup (P2 x startY) digits | x <- [startX ..]]

-- Solving

solve1 :: IntegerMap -> CharMap -> Integer
solve1 numbers symbols = sum $ HM.filterWithKey (adjacent symbols) numbers

solve2 :: CharMap -> IntegerMap -> CharMap -> Integer
solve2 digits numbers symbols = sum $ HM.mapWithKey (gearRatio digits numbers) gears
  where
    gears = HM.filterWithKey (isGear digits) symbols

adjacent :: CharMap -> P -> Integer -> Bool
adjacent symbols (P2 startX startY) n = any (`HM.member` symbols) points
  where
    nLength = length $ show n
    points = neighborsOf startX (startX + nLength - 1) startY

-- Gives the points forming the border of a number between (startX, y) and (endX, y)
neighborsOf :: Int -> Int -> Int -> [P]
neighborsOf startX endX y =
  [P2 x (y - 1) | x <- [(startX - 1) .. (endX + 1)]] ++
  [P2 (startX - 1) y, P2 (endX + 1) y] ++
  [P2 x (y + 1) | x <- [(startX - 1) .. (endX + 1)]]

isGear :: CharMap -> P -> Char -> Bool
isGear digits (P2 x y) c
  | c /= '*' = False
  | otherwise = length (filterDistinct neighbors) == 2
  where
    neighbors = filter (`HM.member` digits) $ neighborsOf x x y

-- Keep p iff p does not have any neighbor to the left of it within the set
filterDistinct :: [P] -> [P]
filterDistinct points = filter (\p -> (p `moveBy` leftV) `notElem` points) points

gearRatio :: CharMap -> IntegerMap -> P -> Char -> Integer
gearRatio digits numbers (P2 x y) _ = case gearNumbers of
  [a, b] -> a * b
  _      -> error "A gear should have exactly two neighbors"
  where
    neighbors = filterDistinct $ filter (`HM.member` digits) $ neighborsOf x x y
    gearNumbers = map (findNumber numbers) neighbors

findNumber :: IntegerMap -> P -> Integer
findNumber numbers (P2 startX y) =
  head $ catMaybes $ [(`HM.lookup` numbers) (P2 x y) | x <- [startX, startX - 1 .. ]]

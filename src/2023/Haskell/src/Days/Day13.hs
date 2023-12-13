module Days.Day13 (solve) where
import           AoCUtils.Days (Solver)
import           Data.Function (on)
import           Data.List     (groupBy, transpose)
import           Data.Maybe    (fromJust)

data Tile = Ash | Rock
  deriving (Show, Eq)

type Pattern = [[Tile]]

solve :: Solver
solve input = let
  patterns = parsePatterns input
  part1 = solve1 patterns
  part2 = solve2 patterns
  in (show part1, show part2)

parsePatterns :: String -> [Pattern]
parsePatterns input = map (map (map parseTile)) chunks
  where
    chunks = filter (/= [""]) $ groupBy ((==) `on` null) $ lines input

parseTile :: Char -> Tile
parseTile '.' = Ash
parseTile '#' = Rock
parseTile _   = error "Illegal char"

solve1 :: [Pattern] -> Integer
solve1 = sum . map (findReflectionValue 0)

solve2 :: [Pattern] -> Integer
solve2 = sum . map (findReflectionValue 1)

findReflectionValue :: Int -> Pattern -> Integer
findReflectionValue target pattern = case findVerticalReflection target pattern of
  Just i  -> toInteger i * 100
  Nothing -> toInteger $ fromJust $ findVerticalReflection target $ transpose pattern

findVerticalReflection :: Int -> Pattern -> Maybe Int
findVerticalReflection _ []            = Nothing
findVerticalReflection target (x : xs) = findVerticalReflection' target [x] xs

findVerticalReflection' :: Int -> [[Tile]] -> [[Tile]] -> Maybe Int
findVerticalReflection' _ _ [] = Nothing
findVerticalReflection' target xRows yRows@(yRow : yRows')
  | differences == target = Just $ length xRows
  | otherwise = findVerticalReflection' target (yRow : xRows) yRows'
  where
    differences = countDifferences xRows yRows

countDifferences :: [[Tile]] -> [[Tile]] -> Int
countDifferences xRows yRows = length $ filter (uncurry (/=)) zipped
  where
    zipped = concat $ zipWith zip xRows yRows

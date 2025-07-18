{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Days.Day25 (solve) where
import           AoCUtils.Days   (Solver)
import           Data.List       (transpose)
import           Data.List.Split (splitOn)

data Schematic = Key [Int] | Lock [Int]
  deriving Show

solve :: Solver
solve input = let
  (schematics, height) = parseSchematics input
  part1 = solve1 schematics height
  in (show part1, "Merry Christmas!")

parseSchematics :: String -> ([Schematic], Int)
parseSchematics input = (schematics, height)
  where
    schematicLines = splitOn [""] $ lines input
    schematics = map parseSchematic schematicLines
    height = length (head $ transpose $ head schematicLines) - 2

parseSchematic :: [String] -> Schematic
parseSchematic schematicLines
  | schematicLines !! 0 !! 0 == '#' = Lock $ parseHeightsFromTop schematicLines
  | otherwise = Key $ parseHeightsFromTop $ reverse schematicLines

parseHeightsFromTop :: [String] -> [Int]
parseHeightsFromTop = map countLength . transpose
  where
    countLength line = length (takeWhile (== '#') line) - 1

solve1 :: [Schematic] -> Int -> Int
solve1 schematics height = length combosThatFit
  where
    (keyHeights, lockHeights) = splitSchematics schematics
    combosThatFit = [(key, lock) | key <- keyHeights, lock <- lockHeights, fits height key lock]

fits :: Int -> [Int] -> [Int] -> Bool
fits maxHeight keyHeights lockHeights =
  and $ zipWith (\keyHeight lockHeight -> keyHeight + lockHeight <= maxHeight) keyHeights lockHeights

splitSchematics :: [Schematic] -> ([[Int]], [[Int]])
splitSchematics = foldr splitSchematic ([], [])
  where
    splitSchematic schematic (keys, locks) = case schematic of
      Key heights  -> (heights : keys, locks)
      Lock heights -> (keys, heights : locks)

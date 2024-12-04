module Days.Day4 (solve) where
import           AoCUtils.Days       (Solver)
import           AoCUtils.Geometry   (Point2 (P2))
import           AoCUtils.Matrices   (matrixToHashMap)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.List           (transpose)
import           Data.Maybe          (fromJust)
import           Text.RE.PCRE.String (RE, compileRegex, countMatches, (*=~))

solve :: Solver
solve input = let
  puzzle = lines input
  part1 = solve1 puzzle
  part2 = solve2 puzzle
  in (show part1, show part2)

solve1 :: [String] -> Int
solve1 puzzle = nHorizontal puzzle + nVertical puzzle + nDiagonal1 puzzle + nDiagonal2 puzzle

xmasRE :: RE
xmasRE = fromJust $ compileRegex "XMAS"

backwards :: [String] -> [String]
backwards = map reverse

nHorizontal' :: [String] -> Int
nHorizontal' = sum . map (\s -> countMatches $ s *=~ xmasRE)

nHorizontal :: [String] -> Int
nHorizontal puzzle = nHorizontal' puzzle + nHorizontal' (backwards puzzle)

nVertical :: [String] -> Int
nVertical = nHorizontal . transpose

nDiagonal1 :: [String] -> Int
nDiagonal1 puzzle = nHorizontal diagonal
  where
    diagonal = transpose $ fst $ foldr indent ([], 0) puzzle

indent :: String -> ([String], Int) -> ([String], Int)
indent s (diagonal, level) = (s' : diagonal, level + 1)
  where
    s' = replicate level '.' ++ s

nDiagonal2 :: [String] -> Int
nDiagonal2 = nDiagonal1 . reverse

solve2 :: [String] -> Int
solve2 puzzle = length $ filter (isXmas charMap) aPoints
  where
    (charMap, _, _) = matrixToHashMap puzzle
    aPoints = HM.keys $ HM.filter (== 'A') charMap

isXmas :: HashMap (Point2 Int) Char -> Point2 Int -> Bool
isXmas aPoints p = isMasDiagonal1 aPoints p && isMasDiagonal2 aPoints p

isMasDiagonal1 :: HashMap (Point2 Int) Char -> Point2 Int -> Bool
isMasDiagonal1 charMap (P2 x y) =
  (c1 == Just 'M' && c2 == Just 'S') ||
  (c1 == Just 'S' && c2 == Just 'M')
  where
    c1 = HM.lookup (P2 (x - 1) (y - 1)) charMap
    c2 = HM.lookup (P2 (x + 1) (y + 1)) charMap

isMasDiagonal2 :: HashMap (Point2 Int) Char -> Point2 Int -> Bool
isMasDiagonal2 charMap (P2 x y) =
  (c1 == Just 'M' && c2 == Just 'S') ||
  (c1 == Just 'S' && c2 == Just 'M')
  where
    c1 = HM.lookup (P2 (x - 1) (y + 1)) charMap
    c2 = HM.lookup (P2 (x + 1) (y - 1)) charMap

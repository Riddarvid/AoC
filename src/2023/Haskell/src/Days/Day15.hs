module Days.Day15 (solve) where
import           AoCUtils.Days     (Solver)
import           Data.Char         (isLetter, ord)
import           Data.Foldable     (foldl')
import           Data.Function     (on)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import           Data.List         (deleteBy, groupBy)
import           Data.Maybe        (fromJust)

data Step = Step {
  sLabel :: String,
  sOp    :: Operation
}

data Operation = Remove | Insert Int

data Lens = Lens {
  lLabel       :: String,
  lFocalLength :: Int
}

solve :: Solver
solve input = let
  stepStrings = parseInput input
  part1 = solve1 stepStrings
  steps = map parseStep stepStrings
  part2 = solve2 steps
  in (show part1, show part2)

parseInput :: String -> [String]
parseInput input = filter (/=",") (groupBy ((==) `on` (== ',')) input')
  where
    input' = head $ lines input

parseStep :: String -> Step
parseStep line = Step label op
  where
    (label, line') = span isLetter line
    op = if head line' == '-'
      then Remove
      else Insert $ read $ tail line'

hash :: String -> Int
hash = foldl' (\acc c -> ((acc + ord c) * 17) `mod` 256) 0

solve1 :: [String] -> Integer
solve1 = sum . map (toInteger . hash)

solve2 :: [Step] -> Integer
solve2 = totalFocusingPower . foldl' executeStep startMap
  where
    startMap = HM.fromList [(n, []) | n <- [0 .. 255]]

executeStep :: HashMap Int [Lens] -> Step -> HashMap Int [Lens]
executeStep boxes (Step label op) = HM.insert boxId box' boxes
  where
    boxId = hash label
    box = fromJust $ HM.lookup boxId boxes
    box' = case op of
      Remove             -> filter (\lens -> lLabel lens /= label) box
      Insert focalLength -> insertOrReplace (Lens label focalLength) box

insertOrReplace :: Lens -> [Lens] -> [Lens]
insertOrReplace lens [] = [lens]
insertOrReplace lens@(Lens label _) (lens'@(Lens label' _) : lenses)
  | label == label' = lens : lenses
  | otherwise = lens' : insertOrReplace lens lenses

totalFocusingPower :: HashMap Int [Lens] -> Integer
totalFocusingPower = sum . HM.mapWithKey focusingPowerBox

focusingPowerBox :: Int -> [Lens] -> Integer
focusingPowerBox boxId lenses = boxFactor * sum (zipWith (*) focalLengths [1 ..])
  where
    boxFactor = toInteger boxId + 1
    focalLengths = map (toInteger . lFocalLength) lenses

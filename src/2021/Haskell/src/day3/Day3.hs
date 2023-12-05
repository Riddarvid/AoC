module Day3 (solve) where

import           Data.List  (transpose)
import           Data.Maybe (fromJust, fromMaybe)

solve :: String -> (Integer, Integer)
solve input = (part1, part2)
  where
    rows = lines input
    part1 = powerConsumption rows
    part2 = lifeSupport rows

-- Part 1

powerConsumption :: [[Char]] -> Integer
powerConsumption rows = binToDec gammaBin * binToDec epsilonBin
  where
    cols = transpose rows
    gammaBin = map (fromJust . mostCommon) cols
    epsilonBin = binNegate gammaBin

mostCommon :: [Char] -> Maybe Char
mostCommon xs
  | nOnes xs * 2 > length xs = Just '1'
  | nOnes xs * 2 < length xs = Just '0'
  | otherwise = Nothing

nOnes :: [Char] -> Int
nOnes []       = 0
nOnes ('1':xs) = 1 + nOnes xs
nOnes ('0':xs) = nOnes xs
nOnes _        = error "Illegal character"

binToDec :: [Char] -> Integer
binToDec bin = binToDec' $ reverse bin

binToDec' :: [Char] -> Integer
binToDec' []       = 0
binToDec' ('1':xs) = 1 + 2 * binToDec' xs
binToDec' ('0':xs) = 2 * binToDec' xs
binToDec' _        = error "Illegal character"

binNegate :: [Char] -> [Char]
binNegate []       = []
binNegate ('1':xs) = '0':binNegate xs
binNegate ('0':xs) = '1':binNegate xs
binNegate _        = error "Illegal character"

-- Part 2

lifeSupport :: [[Char]] -> Integer
lifeSupport rows = binToDec oxygen * binToDec co2
  where
    oxygen = findRating rows oxyBitCrit
    co2 = findRating rows co2BitCrit

findRating :: [[Char]] -> ([[Char]] -> Int -> Char) -> [Char]
findRating rows = findRating' rows 0

findRating' :: [[Char]] -> Int -> ([[Char]] -> Int -> Char) -> [Char]
findRating' [] _ _         = error "No rows left"
findRating' [row] _ _      = row
findRating' rows n bitCrit = findRating' (filter (\row -> (row !! n) == bitToKeep) rows) (n + 1) bitCrit
  where
    bitToKeep = bitCrit rows n

oxyBitCrit :: [[Char]] -> Int -> Char
oxyBitCrit rows n = Data.Maybe.fromMaybe '1' (mostCommon (cols !! n))
  where
    cols = transpose rows

co2BitCrit :: [[Char]] -> Int -> Char
co2BitCrit rows n = case mostCommon (cols !! n) of
  Nothing -> '0'
  Just c  -> if c == '0' then '1' else '0'
  where
    cols = transpose rows

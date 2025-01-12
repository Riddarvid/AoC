module Days.Day22 (solve) where
import           AoCUtils.Days (Solver)
import           Data.Bits     (Bits (xor))
import           Data.Map      (Map)
import qualified Data.Map      as Map

-- 1) For each monkey, compute a list of type [(PriceDiffSeq, Price)]
-- 2) Fold over the list from the left, building a map of type Map PriceDiffSeq Price.
-- an entry is only added the first time a price sequence is encountered.
-- 3) Combine all of the maps, adding together prices when the same key is encountered multiple times.
-- 4) Find the maximum price in the map

solve :: Solver
solve input = let
  initSecrets = map read $ lines input
  secretss = map generateSecrets initSecrets
  part1 = solve1 secretss
  part2 = solve2 secretss
  in (show part1, show part2)

solve1 :: [[Int]] -> Integer
solve1 = sum . map (toInteger . (!! 2000))

generateSecrets :: Int -> [Int]
generateSecrets = iterate generateNextSecret

-- Could be memoized
generateNextSecret :: Int -> Int
generateNextSecret s = s'''
  where
    s' = prune $ mix s $ s * 64
    s'' = prune $ mix s' $ s' `div` 32
    s''' = prune $ mix s'' $ s'' * 2048

mix :: Int -> Int -> Int
mix = xor

prune :: Int -> Int
prune s = s `mod` 16777216

solve2 :: [[Int]] -> Int
solve2 secretss = maximum totalBuyPriceMap
  where
    buyTriggerss = map (computeBuyTriggers 4 . map secretToPrice . take 2001) secretss
    buyPriceMaps = map computeBuyPriceMap buyTriggerss
    totalBuyPriceMap = foldr (Map.unionWith (+)) Map.empty buyPriceMaps

secretToPrice :: Int -> Int
secretToPrice secret = secret `mod` 10

-- Given a list of prices, returns each sequence of 4 price changes and the corresponding buy price
computeBuyTriggers :: Int -> [Int] -> [([Int], Int)]
computeBuyTriggers n prices
  | length priceDiffWindow == n = (priceDiffWindow, price) : computeBuyTriggers n (tail prices)
  | otherwise = []
  where
    priceWindow = take (n + 1) prices
    priceDiffWindow = zipWith (-) (tail priceWindow) priceWindow
    price = last priceWindow

computeBuyPriceMap :: [([Int], Int)] -> Map [Int] Int
computeBuyPriceMap = foldl (\acc (k, v) -> Map.insertWith (\_ old -> old) k v acc) Map.empty

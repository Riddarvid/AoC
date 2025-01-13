module Days.Day23 (solve) where
import           AoCUtils.Days (Solver)
import           Data.List     (intercalate, sort)
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Maybe    (fromJust)
import           Data.Set      (Set)
import qualified Data.Set      as Set

solve :: Solver
solve input = let
  connections = map parseConnection $ lines input
  part1 = solve1 connections
  part2 = solve2 connections
  in (show part1, part2)

parseConnection :: String -> (String, String)
parseConnection connection = (c1, c2)
  where
    (c1, end) = span (/= '-') connection
    c2 = tail end

type ConnectionGroup = Set String

type ConnectionMap = Map String (Set String)

solve1 :: [(String, String)] -> Int
solve1 connections = Set.size $ Set.filter (any (\s -> head s == 't')) $ findConnectionGroups connectionMap
  where
    connectionMap = buildConnectionMap connections

buildConnectionMap :: [(String, String)] -> ConnectionMap
buildConnectionMap = foldr (uncurry appendPair) Map.empty
  where
    appendPair c1 c2 acc = insertConnection c2 c1 $ insertConnection c1 c2 acc
    insertConnection c1 c2 = Map.insertWith Set.union c1 (Set.singleton c2)

findConnectionGroups :: ConnectionMap -> Set ConnectionGroup
findConnectionGroups cm = Set.unions $ map (uncurry (findConnectionGroup cm)) (Map.assocs cm)

findConnectionGroup :: ConnectionMap -> String -> Set String -> Set ConnectionGroup
findConnectionGroup cm c1 = Set.unions . Set.map (findConnectionGroups' cm c1)

findConnectionGroups' :: ConnectionMap -> String -> String -> Set ConnectionGroup
findConnectionGroups' cm c1 c2 = Set.map (\c3 -> Set.fromList [c1, c2, c3]) common'
  where
    c1Set = fromJust $ Map.lookup c1 cm
    c2Set = fromJust $ Map.lookup c2 cm
    common = Set.intersection c1Set c2Set
    common' = Set.delete c1 $ Set.delete c2 common

solve2 :: [(String, String)] -> String
solve2 connections = intercalate "," $ sort $ Set.elems lan
  where
    connectionMap = buildConnectionMap connections
    lan = findLargestSet connectionMap Set.empty $ Map.keys connectionMap

findLargestSet :: ConnectionMap -> Set String -> [String] -> Set String
findLargestSet cm = go
  where
    go lanAcc [] = lanAcc
    go lanAcc (c : cs)
      | isConnectedToAll cm c lanAcc = if Set.size bestWith > Set.size bestWithout then bestWith else bestWithout
      | otherwise = bestWithout
      where
        bestWith = go (Set.insert c lanAcc) cs
        bestWithout = go lanAcc cs

isConnectedToAll :: Ord t => Map t (Set t) -> t -> Set t -> Bool
isConnectedToAll cm c1 = all (`Set.member` c1set)
  where
    c1set = fromJust $ Map.lookup c1 cm

module Day2 (solve) where

solve :: String -> (Integer, Integer)
solve input = (part1 commands, part2 commands)
  where
    commands = parseInput input

-- Setup

type Command = (Direction, Integer)

data Direction = Forward | Up | Down
  deriving (Show)

parseInput :: String -> [Command]
parseInput input = map parseCommand $ lines input

parseCommand :: String -> Command
parseCommand input = case words input of
  [commandStr, distanceStr] -> case commandStr of
      "forward" -> (Forward, read distanceStr)
      "up"      -> (Up, read distanceStr)
      "down"    -> (Down, read distanceStr)
      _         -> error "Parse error"
  _         -> error "Parse error"

-- Part 1

type Position1 = (Integer, Integer)

part1 :: [Command] -> Integer
part1 cmds = horizontal * depth
  where
    (horizontal, depth) = foldl move1 (0, 0) cmds

move1 :: Position1 -> Command -> Position1
move1 (horizontal, depth) (direction, distance) = case direction of
  Forward -> (horizontal + distance, depth)
  Up      -> (horizontal, depth - distance)
  Down    -> (horizontal, depth + distance)

-- Part 2

type Position2 = (Integer, Integer, Integer)

part2 :: [Command] -> Integer
part2 cmds = horizontal * depth
  where
    (horizontal, depth, _) = foldl move2 (0, 0, 0) cmds

move2 :: Position2 -> Command -> Position2
move2 (horizontal, depth, aim) (direction, distance) = case direction of
  Forward -> (horizontal + distance, depth + aim * distance, aim)
  Up      -> (horizontal, depth, aim - distance)
  Down    -> (horizontal, depth, aim + distance)

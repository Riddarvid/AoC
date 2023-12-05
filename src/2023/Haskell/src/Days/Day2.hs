module Days.Day2 (solve) where
import           AoCUtils.Days (Solver)
import           Data.Function (on)
import           Data.Maybe    (fromMaybe)
import           Data.Ord      (comparing)
import           Text.Parsec   (Parsec, char, choice, digit, many1, parse,
                                sepBy1, spaces, string)

solve :: Solver
solve input = let
  games = map parseGame $ lines input
  part1 = solve1 games
  part2 = solve2 games
  in (show part1, show part2)

data Game = Game {
  gId       :: Int,
  gCubeSets :: [CubeSet]
} deriving (Show)

data CubeSet = CS {
  csRed   :: Int,
  csGreen :: Int,
  csBlue  :: Int
} deriving (Show)

-- Parsing

parseGame :: String -> Game
parseGame line = case parse gameParser "" line of
  Left err   -> error $ show err
  Right game -> game

gameParser :: Parsec String () Game
gameParser = do
  _ <- string "Game "
  gId' <- read <$> many1 digit
  _ <- char ':'
  cubeSets <- cubeSetParser `sepBy1` char ';'
  return (Game { gId = gId', gCubeSets = cubeSets})

cubeSetParser :: Parsec String () CubeSet
cubeSetParser = do
  spaces
  colorEntries <- entryParser `sepBy1` char ','
  return $ makeCubeSet colorEntries

entryParser :: Parsec String () (String, Int)
entryParser = do
  spaces
  n <- read <$> many1 digit
  spaces
  color <- choice [string "red", string "green", string "blue"]
  return (color, n)


makeCubeSet :: [(String, Int)] -> CubeSet
makeCubeSet entries = let
  red   = fromMaybe 0 (lookup "red" entries)
  green = fromMaybe 0 (lookup "green" entries)
  blue  = fromMaybe 0 (lookup "blue" entries)
  in CS {csRed = red, csGreen = green, csBlue = blue}

-- Solving

solve1 :: [Game] -> Int
solve1 = sum . map gId . filter (isPossible total)
  where
    total = CS {csRed = 12, csGreen = 13, csBlue = 14}

isPossible :: CubeSet -> Game -> Bool
isPossible total (Game {gCubeSets = cubeSets}) = all (`isSubsetOf` total) cubeSets

isSubsetOf :: CubeSet -> CubeSet -> Bool
isSubsetOf sub super = let
  cmpRed   = comparing csRed sub super
  cmpGreen = comparing csGreen sub super
  cmpBlue  = comparing csBlue sub super
  in notElem GT [cmpRed, cmpGreen, cmpBlue]

solve2 :: [Game] -> Integer
solve2 = sum . map (power . minSet)

minSet :: Game -> CubeSet
minSet (Game {gCubeSets = cubeSets}) = foldr f emptySet cubeSets
  where
    emptySet = CS {csRed = 0, csGreen = 0, csBlue = 0}
    f :: CubeSet -> CubeSet -> CubeSet
    f cs acc = CS {
      csRed = (max `on` csRed) cs acc,
      csGreen= (max `on` csGreen) cs acc,
      csBlue= (max `on` csBlue) cs acc
      }

power :: CubeSet -> Integer
power cs = let
  red = toInteger $ csRed cs
  green = toInteger $ csGreen cs
  blue = toInteger $ csBlue cs
  in red * green * blue

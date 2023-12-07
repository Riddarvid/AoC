module Days.Day7 (solve) where
import           AoCUtils.Days        (Solver)
import           Data.Char            (digitToInt)
import           Data.Function        (on)
import           Data.Functor.Classes (Ord1 (liftCompare))
import           Data.List            (group, sort, sortBy)
import           Data.Ord             (Down (Down), comparing)

solve :: Solver
solve input = let
  hands = map parseLine $ lines input
  part1 = totalWinnings False hands
  part2 = totalWinnings True hands
  in (show part1, show part2)

type Bid = Integer

data Card = Num Int | Ten | Jack | Queen | King | Ace
  deriving (Eq, Ord, Show)

newtype Hand = Hand {
  hCards :: [Card]
} deriving (Eq, Show)

data HandType = HighCard | OnePair | TwoPair | ThreeKind | FullHouse | FourKind | FiveKind
  deriving (Eq, Ord, Show)

-- Parsing

parseLine :: String -> (Hand, Bid)
parseLine line = (Hand $ map parseCard hand', read $ tail bid')
  where
    (hand', bid') = span (/= ' ') line

parseCard :: Char -> Card
parseCard 'A' = Ace
parseCard 'K' = King
parseCard 'Q' = Queen
parseCard 'J' = Jack
parseCard 'T' = Ten
parseCard c   = Num $ digitToInt c

-- General

totalWinnings :: Bool -> [(Hand, Bid)] -> Integer
totalWinnings p2 = sum . zipWith (\rank (_, bid) -> rank * bid) [1 ..] . sortBy (compareHands p2 `on` fst)

compareHands :: Bool -> Hand -> Hand -> Ordering
compareHands p2 hand1 hand2 = case (compare `on` typeOf) hand1 hand2 of
  EQ  -> compareCards compareCard hand1 hand2
  res -> res
  where
    (typeOf, compareCard) = if p2
      then (typeOf2, compareCard2)
      else (typeOf1, compareCard1)

compareCards :: (Card -> Card -> Ordering) -> Hand -> Hand -> Ordering
compareCards cmp (Hand c1) (Hand c2) = liftCompare cmp c1 c2

cardCounts :: [Card] -> [Int]
cardCounts cards = sortBy (comparing Down) (map length $ group $ sort cards)

-- Part 1

typeOf1 :: Hand -> HandType
typeOf1 (Hand cards) = case cardCounts cards of
  5 : _     -> FiveKind
  4 : _     -> FourKind
  3 : 2 : _ -> FullHouse
  3 : _     -> ThreeKind
  2 : 2 : _ -> TwoPair
  2 : _     -> OnePair
  1 : _     -> HighCard
  _         -> error "Invalid hand"

-- Use standard ordering
compareCard1 :: Card -> Card -> Ordering
compareCard1 = compare

-- Part 2

typeOf2 :: Hand -> HandType
typeOf2 (Hand cards)
  | nJokers == 5 || mostCommon + nJokers >= 5 = FiveKind
  | mostCommon + nJokers >= 4 = FourKind
  | mostCommon + secondCommon + nJokers >= 5 = FullHouse
  | mostCommon + nJokers >= 3 = ThreeKind
  | mostCommon + secondCommon + nJokers >= 4 && not (secondCommon == 1 && nJokers == 0) = TwoPair
  | mostCommon + nJokers >= 2 = OnePair
  | otherwise = HighCard
  where
    nJokers = length $ filter (== Jack) cards
    cardCounts' = cardCounts $ filter (/= Jack) cards
    mostCommon = head cardCounts'
    secondCommon = cardCounts' !! 1

-- Use standard ordering except for J
compareCard2 :: Card -> Card -> Ordering
compareCard2 Jack Jack = EQ
compareCard2 Jack _    = LT
compareCard2 _ Jack    = GT
compareCard2 c1 c2     = compare c1 c2

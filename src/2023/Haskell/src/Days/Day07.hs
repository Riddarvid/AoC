module Days.Day07 (solve) where
import           AoCUtils.Days        (Solver)
import           Data.Char            (digitToInt)
import           Data.Function        (on)
import           Data.Functor.Classes (Ord1 (liftCompare))
import           Data.List            (group, sort, sortBy)
import           Data.Ord             (Down (Down), comparing)

solve :: Solver
solve input = let
  hands = map parseLine $ lines input
  part1 = totalWinnings typeOf1 compareCard1 hands
  part2 = totalWinnings typeOf2 compareCard2 hands
  in (show part1, show part2)

type Bid = Integer

data Card = Num Int | T | J | Q | K | A
  deriving (Eq, Ord, Show)

newtype Hand = Hand {
  hCards :: [Card]
} deriving (Eq, Show)

data HandType = HighCard | OnePair | TwoPair | ThreeKind | FullHouse | FourKind | FiveKind
  deriving (Eq, Ord, Show)

type TypeOf = Hand -> HandType

type CardComp = Card -> Card -> Ordering

-- Parsing

parseLine :: String -> (Hand, Bid)
parseLine line = (Hand $ map parseCard hand', read $ tail bid')
  where
    (hand', bid') = span (/= ' ') line

parseCard :: Char -> Card
parseCard 'A' = A
parseCard 'K' = K
parseCard 'Q' = Q
parseCard 'J' = J
parseCard 'T' = T
parseCard c   = Num $ digitToInt c

-- General

totalWinnings :: TypeOf -> CardComp -> [(Hand, Bid)] -> Integer
totalWinnings typeOf cmpCard =
  sum . zipWith (\rank (_, bid) -> rank * bid) [1 ..] .
    sortBy (compareHands typeOf cmpCard `on` fst)

compareHands :: TypeOf -> CardComp -> Hand -> Hand -> Ordering
compareHands typeOf cmpCard hand1 hand2 = case (compare `on` typeOf) hand1 hand2 of
  EQ  -> compareCards cmpCard hand1 hand2
  res -> res

compareCards :: (Card -> Card -> Ordering) -> Hand -> Hand -> Ordering
compareCards cmp = liftCompare cmp `on` hCards

cardCounts :: [Card] -> [Int]
cardCounts cards = sortBy (comparing Down) (map length $ group $ sort cards)

typeOfGeneral :: [Int] -> HandType
typeOfGeneral counts = case counts of
  5 : _     -> FiveKind
  4 : _     -> FourKind
  3 : 2 : _ -> FullHouse
  3 : _     -> ThreeKind
  2 : 2 : _ -> TwoPair
  2 : _     -> OnePair
  1 : _     -> HighCard
  _         -> error "Invalid hand"

-- Part 1

typeOf1 :: Hand -> HandType
typeOf1 = typeOfGeneral . cardCounts . hCards

-- Use standard ordering
compareCard1 :: Card -> Card -> Ordering
compareCard1 = compare

-- Part 2

typeOf2 :: Hand -> HandType
typeOf2 (Hand cards) = typeOfGeneral cardCounts'
  where
    nJokers = length $ filter (== J) cards
    cardCounts' = case cardCounts $ filter (/= J) cards of
      []       -> [nJokers]
      (x : xs) -> x + nJokers : xs

-- Use standard ordering except for J
compareCard2 :: Card -> Card -> Ordering
compareCard2 J J   = EQ
compareCard2 J _   = LT
compareCard2 _ J   = GT
compareCard2 c1 c2 = compare c1 c2

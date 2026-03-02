module Days.Day10.Types (
  Machine(..),
  Button,
  Indicators,
  Joltages
) where
import           Data.HashMap.Lazy (HashMap)

data Machine = Machine {
  mTargetIndicators :: Indicators,
  mButtonSchematics :: [Button],
  mJoltageReqs      :: Joltages
} deriving (Show)

type Button = [Int]

type Indicators = HashMap Int Bool

type Joltages = HashMap Int Int

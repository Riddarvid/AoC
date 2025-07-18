{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Days.Day24 (solve) where
import           AoCUtils.Days        (Solver)
import           Control.Monad        (filterM)
import           Control.Monad.Reader (MonadReader (ask), Reader, runReader)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HM
import           Data.List            (intercalate, sort)
import           Data.List.Split      (splitOn)
import           Data.Maybe           (catMaybes, isNothing)

data Gate = Gate Operator String String
  deriving Show

data Operator = AND | OR | XOR
  deriving (Show, Eq)

solve :: Solver
solve input = let
  (wires, gates, n) = parseInput input
  part1 = solve1 wires gates n
  part2 = solve2 gates n
  in (show part1, part2)

-- Parsing

parseInput :: String -> (HashMap String Bool, HashMap String Gate, Int)
parseInput input = (wires, gates, maxN)
  where
    iLines = lines input
    (wireLines, gateLines) = span (/= "") iLines
    gateLines' = tail gateLines
    wires = parseWires wireLines
    gates = parseGates gateLines'
    zGateMap = HM.filterWithKey (\k _ -> head k == 'z') gates
    zKeys = sort $ HM.keys zGateMap
    maxKey = last zKeys
    maxN = read $ tail maxKey

parseWires :: [String] -> HashMap String Bool
parseWires = HM.fromList . map parseWire

parseWire :: String -> (String, Bool)
parseWire input = (name, val)
  where
    (name, input') = span (/= ':') input
    val = case input' !! 2 of
      '0' -> False
      '1' -> True
      c   -> error $ "Undefined value " ++ [c]

parseGates :: [String] -> HashMap String Gate
parseGates = HM.fromList . map parseGate

parseGate :: String -> (String, Gate)
parseGate input = (name, Gate operator operand1 operand2)
  where
    tokens = splitOn " " input
    operand1 = tokens !! 0
    operand2 = tokens !! 2
    operator = parseOperator (tokens !! 1)
    name = tokens !! 4

parseOperator :: String -> Operator
parseOperator = \case
  "AND" -> AND
  "OR" -> OR
  "XOR" -> XOR
  s -> error $ "Undefinded operator " ++ s

-- Part 1

solve1 :: HashMap String Bool -> HashMap String Gate -> Int -> Integer
solve1 varMap gateMap maxN = binToDec zBinary
  where
    zVars = map (var "z") [0 .. maxN]
    zBinary = map (eval varMap gateMap) zVars

binToDec :: [Bool] -> Integer
binToDec = foldr (\b acc -> 2 * acc + if b then 1 else 0) 0

eval :: HashMap String Bool -> HashMap String Gate -> String -> Bool
eval varMap gateMap = go
  where
    go wire = case HM.lookup wire varMap of
      Just v -> v
      Nothing -> case HM.lookup wire gateMap of
        Nothing             -> error $ "Can't find wire" ++ wire
        Just (Gate operator operand1 operand2) -> let
          res1 = go operand1
          res2 = go operand2
          in case operator of
            AND -> res1 && res2
            OR  -> res1 || res2
            XOR -> res1 `xor` res2
      where
        xor p q = (p && not q) || (q && not p)

-- Part 2

solve2 :: HashMap String Gate -> Int -> String
solve2 gateMap maxN = intercalate "," $ sort swappedWires
  where
    (swaps, _) = doAllSwaps gateMap maxN
    swappedWires = concatMap (\(w1, w2) -> [w1, w2]) swaps

doAllSwaps :: HashMap String Gate -> Int -> ([(String, String)], HashMap String Gate)
doAllSwaps _gateMap maxN = go _gateMap
  where
    go gateMap = case findFirstSwap gateMap maxN of
      Nothing       -> ([], gateMap)
      Just (w1, w2) -> let
        (swaps, gateMap') = go (swapOutputWires w1 w2 gateMap)
        in ((w1, w2) : swaps, gateMap')

findFirstSwap :: HashMap String Gate -> Int -> Maybe (String, String)
findFirstSwap gateMap maxN = case catMaybes errors of
  (w1, targetPred) : _ -> case runFindMatching gateMap targetPred of
    Just w2 -> Just (w1, w2)
    Nothing -> error "Could not find wire to swap with."
  [] -> Nothing
  where
    -- Special case for last bit since it has no inputs and is therefore only determined by the n-1 carry bit
    errors = map (runIdentifyResError gateMap) [0 .. maxN - 1] ++
      [runIdentifyError gateMap (carryGatePred (maxN - 1)) (var "z" maxN)]

-- GateReader functions ---------------

newtype GateData = GD (HashMap String Gate)

type GateReader = Reader GateData

data GatePred = GPVar String | GPGate Operator GatePred GatePred
  deriving (Show)

runFindMatching :: HashMap String Gate -> GatePred -> Maybe String
runFindMatching gateMap gatePred = runReader (findMatching gatePred) (GD gateMap)

runIdentifyError :: HashMap String Gate -> GatePred -> String -> Maybe (String, GatePred)
runIdentifyError gateMap gatePred gateName =
  runReader (identifyError gatePred gateName) (GD gateMap)

runIdentifyResError :: HashMap String Gate -> Int -> Maybe (String, GatePred)
runIdentifyResError gateMap n = runIdentifyError gateMap (resGatePred n) (var "z" n)

findMatching :: GatePred -> GateReader (Maybe String)
findMatching gatePred = do
  GD gateMap <- ask
  matching <- filterM (gateMatchesPred gatePred) $ HM.keys gateMap
  case matching of
    []  -> return Nothing
    [x] -> return $ Just x
    _   -> error "Did not expect multiple identical functions"

-- Returns the first error, and what it should have been.
-- An error is identified as follows:
-- If operator is wrong, then gate is wrong
-- If both operands are wrong, then gate is wrong. This is not guaranteed in the general
-- case, but hopefully it holds true for this problem.
-- If only one operand is wrong, we return the error found in that path.
identifyError :: GatePred -> String -> GateReader (Maybe (String, GatePred))
identifyError gatePred gateName = case gatePred of
  GPVar varName -> if gateName == varName
    then return Nothing
    else return errorObject
  GPGate operator p1 p2 -> do
    GD gateMap <- ask
    case HM.lookup gateName gateMap of
      Nothing                                 -> return errorObject
      Just (Gate operator' operand1 operand2) -> if operator /= operator'
        then return errorObject
        -- Order does not matter so we have to check both possible configurations
        else do
          res1 <- identifyError p1 operand1
          res2 <- identifyError p2 operand2
          case (res1, res2) of
            (Nothing, Nothing) -> return Nothing
            (Nothing, err)     -> return err
            (err, Nothing)     -> return err
            (_, _)             -> do
              res1' <- identifyError p1 operand2
              res2' <- identifyError p2 operand1
              case (res1', res2') of
                (Nothing, Nothing) -> return Nothing
                (Nothing, err)     -> return err
                (err, Nothing)     -> return err
                (_, _)             -> return errorObject
  where
    errorObject = Just (gateName, gatePred)

gateMatchesPred :: GatePred -> String -> GateReader Bool
gateMatchesPred gatePred gateName = isNothing <$> identifyError gatePred gateName

-- Predicates ---------------------------

resGatePred :: Int -> GatePred
resGatePred n
 | n == 0 = varGatePred n XOR
 | otherwise = GPGate XOR (carryGatePred (n - 1)) (varGatePred n XOR)

carryGatePred :: Int -> GatePred
carryGatePred n
  | n == 0 = varGatePred n AND
  | otherwise = GPGate OR
    (varGatePred n AND)
    (GPGate AND
      (varGatePred n XOR)
      (carryGatePred (n - 1)))

varGatePred :: Int -> Operator -> GatePred
varGatePred n operator = GPGate operator (xPred n) (yPred n)

xPred :: Int -> GatePred
xPred = GPVar . var "x"

yPred :: Int -> GatePred
yPred = GPVar . var "y"

-- Utils -----------------------------

var :: String -> Int -> String
var prefix n
  | n < 10 = prefix ++ "0" ++ show n
  | otherwise = prefix ++ show n

swapOutputWires :: String -> String -> HashMap String Gate -> HashMap String Gate
swapOutputWires g1 g2 = HM.mapKeys swapFun
  where
    swapFun g
      | g == g1 = g2
      | g == g2 = g1
      | otherwise = g

-- Used in exploration but not in final solution ---------------------

showGate :: String -> HashMap String Gate -> String
showGate = showGate' 0

showGate' :: Int -> String -> HashMap String Gate -> String
showGate' depth key gateMap = case HM.lookup key gateMap of
  Nothing                              -> padding ++ key -- variable
  Just (Gate operator input1 input2) -> let
    inputString1 = showGate' (depth + 1) input1 gateMap
    inputString2 = showGate' (depth + 1) input2 gateMap
    in padding ++ key ++ ": " ++ show operator ++ "\n" ++
      inputString1 ++ "\n" ++
      inputString2
  where
    indent = "  "
    padding = concat $ replicate depth indent

showBoolExpr :: String -> HashMap String Gate -> String
showBoolExpr key gateMap = case HM.lookup key gateMap of
  Nothing                                  -> key
  Just (Gate operator operand1 operand2) -> let
    operandString1 = showBoolExpr operand1 gateMap
    operandString2 = showBoolExpr operand2 gateMap
    in "(" ++ operandString1 ++ " " ++ show operator ++ " " ++ operandString2 ++ ")"

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
module Days.Day20 (solve) where
import           AoCUtils.Days              (Solver)
import           Control.Monad              (replicateM_)
import           Control.Monad.State.Strict (State, evalState, execState, gets,
                                             modify)
import           Data.Foldable              (traverse_)
import           Data.Hashable              (Hashable)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HM
import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as HS
import           Data.List                  (sort)
import           GHC.Generics               (Generic)
import           Text.Parsec                (Parsec, char, letter, many1, parse,
                                             sepBy, string, (<|>))

-- Notes
-- Must keep track of order of signals. Might be good to maintain a list or queue of transmissions
-- consisting of a pulse value, source, and destination.

-- A & must know all its inputs beforehand - keep in a hashmap

data ModuleEntry = ModuleEntry {
  _meType   :: ModuleEntryType,
  meName    :: String,
  meOutputs :: [String]
}

data ModuleEntryType = MEFlipFlop | MEConjunction | MEBroadcast

type Modules = HashMap String Module

data Module = Module String ModuleType [String]
  deriving (Show, Eq, Generic)

instance Hashable Module

data ModuleType = FlipFlop Bool | Conjunction (HashMap String Bool) | Broadcast
  deriving (Show, Eq, Generic)

instance Hashable ModuleType

type Parser = Parsec String ()

solve :: Solver
solve input = let
  modules = parseInput input
  part1 = solve1 modules
  part2 = solve2 modules
  in (show part1, show part2)

-- Parsing

parseInput :: String -> Modules
parseInput input = HM.fromList $ map (mkModule moduleEntries) moduleEntries
  where
    moduleEntries = map parseModule $ lines input

mkModule :: [ModuleEntry] -> ModuleEntry -> (String, Module)
mkModule moduleEntries (ModuleEntry meType' name outputs) = (name, Module name mType outputs)
  where
    mType = case meType' of
      MEBroadcast   -> Broadcast
      MEFlipFlop    -> FlipFlop False
      MEConjunction -> Conjunction $ HM.fromList $ map (, False) inputs
    inputs = map meName $ filter (\me -> name `elem` meOutputs me) moduleEntries

parseModule :: String -> ModuleEntry
parseModule line = case parse moduleParser "" line of
  Left err  -> error $ show err
  Right res -> res

moduleParser :: Parser ModuleEntry
moduleParser = do
  moduleType <- MEFlipFlop <$ char '%' <|> MEConjunction <$ char '&' <|> return MEBroadcast
  name <- many1 letter
  _ <- string " -> "
  destinations <- many1 letter `sepBy` string ", "
  return $ ModuleEntry moduleType name destinations

-- Part 1

data ModulesState = MS {
  msLow     :: Integer,
  msHigh    :: Integer,
  msPushes  :: Integer,
  msModules :: Modules,
  msQueue   :: Queue Signal
}

data Signal = Signal String String Bool

data Queue a = Queue [a] [a]

solve1 :: Modules -> Integer
solve1 modules = msLow endState * msHigh endState
  where
    push1000 = replicateM_ 1000 pushButton
    endState = execState push1000 (mkState modules)

solve2 :: Modules -> Integer
solve2 modules = foldr lcm 1 cycleLengths
  where
    outs = ["cl", "rp", "lb", "nj"]
    cycleLengths = map (toInteger . (`findCycleLength` modules)) outs

findCycleLength :: String -> Modules -> Int
findCycleLength out modules = findCycleLength' modules'
  where
    ins = findInputs out modules
    modules' = HM.filterWithKey (\name _ -> name `elem` ins) modules

findInputs :: String -> Modules -> [String]
findInputs name = sort . HS.toList . findInputs' (HS.singleton name) name

findInputs' :: HashSet String -> String -> Modules -> HashSet String
findInputs' found name modules = foldr (\x acc -> findInputs' acc x modules) found' new
  where
    inputs = HM.keysSet $
      HM.filter (\(Module _ _ outputs) -> name `elem` outputs) modules
    new = HS.difference inputs found
    found' = HS.union new found

findCycleLength' :: Modules -> Int
findCycleLength' modules = length moduless - 1
  where
    moduless = evalState (pushUntilReset HS.empty) (mkState modules)

pushUntilReset :: HashSet Modules -> State ModulesState [Modules]
pushUntilReset seen = do
  modules <- gets msModules
  if HS.member modules seen
    then return []
    else do
      pushButton
      (modules :) <$> pushUntilReset (HS.insert modules seen)

mkState :: Modules -> ModulesState
mkState modules = MS {
  msLow = 0,
  msHigh = 0,
  msPushes = 0,
  msModules = modules,
  msQueue = Queue [] []
}

pushButton :: State ModulesState ()
pushButton = do
  modify (\s -> s{msPushes = msPushes s + 1})
  sendSignal $ Signal "button" "broadcaster" False
  processSignals

processSignals :: State ModulesState ()
processSignals = do
  mHead <- msPop
  case mHead of
    Nothing -> return ()
    Just signal -> do
      processSignal signal
      processSignals

processSignal :: Signal -> State ModulesState ()
processSignal signal = do
  destModule <- getDestModule signal
  case destModule of
    Nothing          -> return ()
    Just destModule' -> processSignal' destModule' signal

processSignal' :: Module -> Signal -> State ModulesState ()
processSignal' module'@(Module _ mType _) = process module'
  where
    process = case mType of
      Broadcast            -> processBroadcast
      FlipFlop ffState     -> processFlipFlop ffState
      Conjunction conState -> processConjunction conState

processBroadcast :: Module -> Signal -> State ModulesState ()
processBroadcast module' (Signal _ _ val) = sendToOutputs module' val

processFlipFlop :: Bool -> Module -> Signal -> State ModulesState ()
processFlipFlop _ _ (Signal _ _ True) = return ()
processFlipFlop ffState module' _ = do
  let ffState' = not ffState
  updateFlipFlop module' ffState'
  sendToOutputs module' ffState'

processConjunction :: HashMap String Bool -> Module -> Signal -> State ModulesState ()
processConjunction conState module' (Signal src _ val) = do
  let conState' = HM.insert src val conState
  updateConjunction module' conState'
  let outVal = not $ and conState'
  sendToOutputs module' outVal

updateFlipFlop :: Module -> Bool -> State ModulesState ()
updateFlipFlop (Module name _ outputs) val = do
  modules <- gets msModules
  let modules' = HM.insert name (Module name (FlipFlop val) outputs) modules
  modify (\s -> s{msModules = modules'})

updateConjunction :: Module -> HashMap String Bool -> State ModulesState ()
updateConjunction (Module name _ outputs) val = do
  modules <- gets msModules
  let modules' = HM.insert name (Module name (Conjunction val) outputs) modules
  modify (\s -> s{msModules = modules'})

sendToOutputs :: Module -> Bool -> State ModulesState ()
sendToOutputs (Module name _ outputs) val = traverse_ sendSignal signals
  where
    signals = map (\dest -> Signal name dest val) outputs

-- Might be better to add untyped modules as a module type?
getDestModule :: Signal -> State ModulesState (Maybe Module)
getDestModule (Signal _ dest _) = do
  modules <- gets msModules
  return $ HM.lookup dest modules

sendSignal :: Signal -> State ModulesState ()
sendSignal signal@(Signal _ _ high) = do
  modify (\s -> s{msQueue = push signal $ msQueue s})
  if high
    then modify (\s -> s{msHigh = msHigh s + 1})
    else modify (\s -> s{msLow = msLow s + 1})

msPop :: State ModulesState (Maybe Signal)
msPop = do
  queue <- gets msQueue
  let (head', queue') = pop queue
  modify (\s -> s{msQueue = queue'})
  return head'

push :: a -> Queue a -> Queue a
push x (Queue eq dq) = Queue (x : eq) dq

-- Reverses eq when dq is empty
pop :: Queue a -> (Maybe a, Queue a)
pop (Queue [] [])       = (Nothing, Queue [] [])
pop (Queue eq [])       = pop $ Queue [] (reverse eq)
pop (Queue eq (x : dq)) = (Just x, Queue eq dq)

module Days.Day9 (solve) where
import           AoCUtils.Days (Solver)
import           Data.Char     (digitToInt)
import           Data.Sequence (Seq (Empty, (:<|), (:|>)))

-- 20000 block segments in input

solve :: Solver
solve input = let
  drive = parseDrive input
  part1 = solve1 drive
  part2 = solve2 drive
  in (show part1, show part2)

data Block = Free | File Int
  deriving (Show)

type Drive = Seq (Block, Int)

parseDrive :: String -> Drive
parseDrive input = drive
  where
    ints = map digitToInt input
    (drive, _, _) = foldl buildDrive (Empty, True, 0) ints

buildDrive :: (Drive, Bool, Int) -> Int -> (Drive, Bool, Int)
buildDrive (accDrive, isFile, nextFileId) size = (accDrive', not isFile, nextFileId')
  where
    (blockElem, nextFileId') = if isFile
      then (File nextFileId, nextFileId + 1)
      else (Free, nextFileId)
    accDrive' = accDrive :|> (blockElem, size)

solve1 :: Drive -> Integer
solve1 = checksum . compactDrive

solve2 :: Drive -> Integer
solve2 = checksum . defragDrive

-- Compacting

compactDrive :: Drive -> Drive
compactDrive Empty                   = Empty
compactDrive (drive :|> (Free, _)) = compactDrive drive
compactDrive ((File fileId, size) :<| drive) = (File fileId, size) :<| compactDrive drive
compactDrive (startDrive :|> (File fileId, fileSize))
  | sizeDiff > 0 = (File fileId, fileSize) :<| compactDrive ((Free, sizeDiff) :<| middleDrive) -- Entire end block fits
  | otherwise = (File fileId, freeSize) :<| compactDrive (middleDrive :|> (File fileId, fileSize - freeSize)) -- Only some of the block fits
  where
    (freeSize, middleDrive) = case startDrive of
      ((Free, freeSize') :<| middleDrive') -> (freeSize', middleDrive')
      _                                    -> error "Should not happen"
    sizeDiff = freeSize - fileSize

-- Formatting

defragDrive :: Drive -> Drive
defragDrive Empty                 = Empty
defragDrive (drive :|> block@(Free, _)) = defragDrive drive :|> block
defragDrive (block@(File _, _) :<| drive) = block :<| defragDrive drive
defragDrive (startDrive :|> endBlock@(File fileId, fileSize))
  = case tryInsertFile fileId fileSize startDrive of
    Nothing     -> defragDrive startDrive :|> endBlock
    Just drive' -> defragDrive (drive' :|> (Free, fileSize))

tryInsertFile :: Int -> Int -> Seq (Block, Int) -> Maybe Drive
tryInsertFile fileId fileSize = go
  where
    go drive = case drive of
      Empty                             -> Nothing
      block@(File _, _) :<| drive'      -> (block :<|) <$> go drive'
      block@(Free, freeSize) :<| drive' -> case compare diffSize 0 of
        EQ -> Just $ (File fileId, fileSize) :<| drive'
        GT -> Just $ (File fileId, fileSize) :<| (Free, diffSize) :<| drive'
        LT -> (block :<|) <$> go drive'
        where
          diffSize = freeSize - fileSize

-- Checksum

checksum :: Drive -> Integer
checksum = fst . foldl appendChecksum (0, 0)

appendChecksum :: (Integer, Integer) -> (Block, Int) -> (Integer, Integer)
appendChecksum (acc, index) (block, size) = case block of
  Free        -> (acc, index')
  File fileId -> let acc' = acc + segmentSum fileId index size in (acc', index')
  where
    index' = index + toInteger size

segmentSum :: Int -> Integer -> Int -> Integer
segmentSum fileId index size = toInteger fileId * aritSum
  where
    aritSum = size' * (index + (index + size' - 1)) `div` 2
    size' = toInteger size

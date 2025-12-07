module Utils (
  split,
  chunks,
  flattenSingletonPartial
) where

split :: Eq a => a -> [a] -> [[a]]
split delim xs = case remainder of
  []        -> [start]
  (_ : xs') -> start : split delim xs'
  where
    (start, remainder) = span (/= delim) xs

chunks :: Int -> [a] -> [[a]]
chunks chunkLength xs = case xs' of
  [] -> [chunk]
  _  -> chunk : chunks chunkLength xs'
  where
    (chunk, xs') = splitAt chunkLength xs

flattenSingletonPartial :: [a] -> a
flattenSingletonPartial [x] = x
flattenSingletonPartial _   = error "List not singleton"

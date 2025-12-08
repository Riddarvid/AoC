module Utils (
  split,
  chunks,
  flattenSingletonPartial,
  iterateEmit,
  generalRecursionStep
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

iterateEmit :: (a -> (a, b)) -> a -> b -> [(a, b)]
iterateEmit f x y = (x, y) : iterateEmit'  f x

iterateEmit' :: (a -> (a, b)) -> a -> [(a, b)]
iterateEmit' f x = (x', y') : iterateEmit' f x'
  where
    (x', y') = f x

generalRecursionStep ::
  (a -> Maybe b) ->
  (a -> [a]) ->
  (a -> [b] -> b) ->
  (a -> b) ->
  (a -> b)
generalRecursionStep stopCond splitProblem combineSolutions rec' x =
  case stopCond x of
    Just y -> y
    Nothing -> let
      subProblems = splitProblem x
      subSolutions = map rec' subProblems
      solution = combineSolutions x subSolutions
      in solution

module Day4 (rows) where

import           Data.List.Utils
import           Data.Map        (Map)
import qualified Data.Map        as Map

-- Bingo boards

type Position = (Int, Int)

type Square = (Position, Bool)

type Board = Map Integer Square

playSquares :: Board -> [Integer] -> (Board, Integer)
playSquares board []     = (board, 0)
playSquares board _
  | boardWon board = (board, 0)
playSquares board (n:ns) = (board'', rounds + 1)
  where
    board' = playSquare board n
    (board'', rounds) = playSquares board' ns

playSquare :: Board -> Integer -> Board
playSquare board n = Map.adjust (\(pos, _) -> (pos, True)) n board

boardWon :: Board -> Bool
boardWon board = any (`contains` filled) bingolines
  where
    filled = map fst (filter snd (Map.elems board))

bingolines :: [[Position]]
bingolines = rows ++ cols ++ diagonals

rows :: [[Position]]
rows = [[(x, y) | x <- [0..4]] | y <- [0..4]]

cols :: [[Position]]
cols = [[(x, y) | y <- [0..4]] | x <- [0..4]]

diagonals :: [[Position]]
diagonals = [(x, x) | x <- [0..4]] : [[(x, 4 - x) | x <- [0..4]]]

module Hazi8 where

wave :: Integer -> [Integer]
wave n =
  [x | x <- [0 .. n]]
    ++ [x | x <- [n - 1, n - 2 .. -n]]
    ++ [x | x <- [-n + 1 .. -1]]

waves :: Integer -> [Integer]
waves n = [x | i <- [1 .. n], x <- wave i]

chessboardFields :: [(Int, Char)]
chessboardFields = [(row, column) | row <- [1 .. 8], column <- ['a' .. 'h']]

rookMoves :: (Int, Char) -> [(Int, Char)]
rookMoves (row, column) = [(newRow, column) | newRow <- [1 .. 8], newRow /= row] ++ [(row, newColumn) | newColumn <- ['a' .. 'h'], newColumn /= column]

bishopMoves :: (Int, Char) -> [(Int, Char)]
bishopMoves (row, column) = [(newRow, newColumn) | (newRow, newColumn) <- chessboardFields, (newRow, newColumn) /= (row, column), abs (newRow - row) == abs (fromEnum newColumn - fromEnum column)]
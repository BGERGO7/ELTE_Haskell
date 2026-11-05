module Hazi3 where

indexOfArg :: Integer -> Integer -> Integer -> Integer
indexOfArg 0 _ _ = 1
indexOfArg _ 0 _ = 2
indexOfArg _ _ 0 = 3
indexOfArg _ _ _ = -1

doOverlap :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
doOverlap x1 y1 x2 y2 x3 y3 x4 y4 = not (x2 < x3 || x4 < x1 || y2 < y3 || y4 < y1)

doubleFact :: Integer -> Integer
doubleFact 0 = 1 
doubleFact 1 = 1
doubleFact x = x * doubleFact(x - 2)

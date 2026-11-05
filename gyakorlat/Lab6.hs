module Lab6 where

import Data.Char

myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake n (x:xs)
   | n <= 0 = []
   | otherwise = x : myTake (n-1) xs

myReplicate :: Int -> a -> [a]
myReplicate n e
   | n <= 0 = []
   | otherwise = e : myReplicate (n-1) e

myRepeat :: a -> [a]
myRepeat x = x : myRepeat x

onlyUppers :: String -> String
onlyUppers "" = ""
onlyUppers (ch:chs)
   | isUpper ch = ch : onlyUppers chs
   | otherwise = onlyUppers chs

infixr 5 +:+
(+:+) :: [a] -> [a] -> [a]
[] +:+ ys = ys
(x:xs) +:+ ys = (x: xs +:+ ys)


sortedMerge :: Ord a => [a] -> [a] -> [a]
sortedMerge [] ys = ys
sortedMerge xs [] = xs
sortedMerge (x:xs) (y:ys)
   | x < y = x : sortedMerge xs (y:ys)
   | otherwise = y : sortedMerge (x:xs) ys 

divByN :: Integer -> [Integer]
divByN n
  | n<=0 = []
  | otherwise = [0,n..]

ithInSeq :: Integer -> Integer -> Int -> Integer
ithInSeq x y n = [x,y..] !! (n-1)

nthLetter :: Int -> Bool -> Char
nthLetter n True | n >= 0 && n<= 25 = ['A'..'Z'] !! n
nthLetter n False | n >= 0 && n<= 25 = ['a'..'z'] !! n


-- rendezett n esek

first :: (a, b) -> a
first (x, y) = x

second :: (a, b) -> b
second (x,y) = y

swap :: (a, b) -> (b, a)
swap (x,y) = (y,x)

mirrorX :: (Integer, Integer) -> (Integer, Integer)
mirrorX (x,y) = (x,-y)

scale' :: Integer -> (Integer, Integer) -> (Integer, Integer)
scale' d (x,y) = (x*d,y*d)

mirrorP :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
mirrorP (x1, y1) (x2, y2) = (2 * x1 - x2, 2 * y1 - y2)

distance :: (Double, Double) -> (Double, Double) -> Double
distance (x1, y1) (x2, y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

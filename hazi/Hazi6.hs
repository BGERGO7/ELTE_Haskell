module Hazi6 where

mirrorP :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
mirrorP (x1, y1) (x2, y2) = (2 * x1 - x2, 2 * y1 - y2)

range :: [Integer] -> [Integer]
range [] = []
range [x] = [x..]
range [x, y] = [x, y..]
range (x:y:z:_)
    | x > y && x < z = []
    | x == y && x == z = [x]
    | otherwise = [x, y..z]

dupElems :: [a] -> [a]
dupElems [] = []
dupElems (x:xs) = x : x : dupElems xs

elemIds :: Eq a => a -> [a] -> [Int]
elemIds _ [] = []
elemIds n (x:xs) = elemIdsR n 0 (x:xs)

elemIdsR _ _ [] = []
elemIdsR n i (x:xs)
    | n == x = i : elemIdsR n (i + 1) xs
    | otherwise = elemIdsR n (i + 1) xs
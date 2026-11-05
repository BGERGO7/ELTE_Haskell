module Hazi4 where
coneSurface :: Double -> Double -> Double
coneSurface r h = pi * r * (r + sqrt(h^2 + r^2))

discount :: Double -> Double
discount x
    | x <= 50000 = x
    | x > 50000 && x <= 100000 = x * 0.95
    | x > 100000 && x <= 500000 = x * 0.9
    | otherwise = x * 0.85

geomTerm :: Double -> Double -> Int -> Double
geomTerm kezdo kvociens elem
    | elem <= 1 = kezdo
    | otherwise = kvociens * geomTerm kezdo kvociens (elem - 1)

sumOfDivisors :: Integer -> Integer
sumOfDivisors n
    | n <= 0 = 0
    | otherwise = sumDivisorsR n n

sumDivisorsR :: Integer -> Integer -> Integer
sumDivisorsR x y
    | y == 0 = 0
    | x `mod` y == 0 = y + sumDivisorsR x (y - 1)
    | otherwise = sumDivisorsR x (y - 1)
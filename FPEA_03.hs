module FPEA_02 where --Modul neve = fajl neve

-- import Data.Char 


--average :: Double -> Double -> Double
--average :: Float -> Float -> Float

--average2 :: [Int] -> Double
average2 :: (Integral a, Fractional b) => [a] -> b
average2 l = fromIntegral (sum l) / fromIntegral (length l)

-- :: [Int] -> Double
-- :: [Integer] -> Double
-- :: [Integer] -> Float

-- 3! -> 1 * 2 * 3
-- 3! 3 * 2!

fact :: Integer -> Integer
fact 0 = 1
fact i = i * fact (i - 1)

fact2 :: Integer -> Integer -> Integer
fact2 i m
    | i == m = i
    | i < m = i * fact2 (i+1) m
    | 

twos = 2 : twos

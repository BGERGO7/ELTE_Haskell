import Data.Char
import Distribution.Compat.CharParsing (chainl)
import Distribution.Simple.Utils (xargs)

sphereVolume :: Double -> Double
sphereVolume r = (4 * r^3 * pi) / 3

myAbs :: Integer -> Integer
myAbs x
     | x < 0 = -x
     | otherwise = x

sign :: Integer -> Integer
sign 0 = 0
sign x 
    | x < 0 = -1
    | otherwise = 1

swapUpperLower :: Char -> Char
swapUpperLower ch
    | isUpper ch = toLower ch
    | isLower ch = toUpper ch
    | otherwise = ch


fact :: Integer -> Integer
fact 0 = 1
fact x
    | x > 0 = x * fact(x - 1)


sumTo :: Integer -> Integer
sumTo 0 = 0
sumTo n 
    | n < 0 = 0
    | otherwise = n + sumTo(n - 1)

sumBetween :: Integer -> Integer -> Integer
sumBetween n m
    | n <= m = sumBetweenR n m
    | otherwise = sumBetweenR m n

sumBetweenR n m
    | n == m = n
    | otherwise = m + sumBetweenR n (m - 1)

sqr :: Integer -> Integer
sqr x = x^2

pwr :: Integer -> Integer -> Integer
pwr x n
    | x == 0 = 1
    | odd n = x * pwr x (n - 1)
    | otherwise = sqr(pwr(x (n `div` 2)))
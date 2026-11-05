module Lab3 where

--(3 :: Int) + (4 :: Int)
--7 :: Int

--3 + 4
--7 :: Integer


--konstans
secondsInLeapYear :: Integer
secondsInLeapYear = 366 * 24 * 60 * 60

double :: Integer -> Integer
double x = x * 2

isEven :: Integer -> Bool
--isEven x = even(x)
isEven x = x `mod` 2 == 0

isOdd :: Integer -> Bool
--isOdd x = x `mod` 2 /= 0
--isOdd x = odd(x)
isOdd x = not(isEven x)

divides :: Integer -> Integer -> Bool
divides x y = y `mod` x == 0

triangleSides :: Integer -> Integer -> Integer -> Bool
triangleSides a b c = (a+b) > c && (a+c) > b && (c+b) > a

isLeapYear :: Integer -> Bool
isLeapYear x = 4 `divides` x
    && not (100 `divides` x)
    || 400 `divides` x

isZero :: Integer -> Bool
isZero 0 = True
isZero _ = False

mul3 :: Int -> Int -> Int
mul3 0 _ = 0
mul3 _ 0 = 0
mul3 1 x = x
mul3 x 1 = x
mul3 2 2 = 1

nextDay :: Integer -> Integer
nextDay 6 = 0
nextDay x = x + 1


--Rekurzio

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1)

{-

fact 5
2    5 * fact 4
3        4 * fact 3
4            3 * fact 2
5                2 * fact 1
6                    1 * fact 0
7                        1
8                    1
9                2
10           6
11       24
12   120
-}

sumTo :: Integer -> Integer
sumTo 0 = 0
sumTo x = x + sumTo(x - 1)

multiply :: Integer -> Integer -> Integer
multiply 0 y = 0
multiply x y = y + multiply (x - 1) y

sumSquaresTo :: Integer -> Integer
sumSquaresTo 0 = 0
sumSquaresTo x = x*x + sumSquaresTo(x - 1)


-- nem jou
powerN :: Integer -> Integer -> Integer
powerN 0 b = 0
powerN b 0 = 1
powerN a b = a*b

fibonacci :: Integer -> Integer
fibonacci 0=0
fibonacci 1=1
fibonacci x = fibonacci(x - 1) + fibonacci(x - 2)
 

tribonacci :: Integer -> Integer
tribonacci 0 = 0
tribonacci 1 = 0
tribonacci 2 = 1
tribonacci x = tribonacci(x - 1) + tribonacci(x - 2) + tribonacci(x - 3)


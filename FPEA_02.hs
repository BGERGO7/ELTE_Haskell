module FPEA_02 where --Modul neve = fajl neve

-- data Bool = False | True

--incInt :: Int -> Int
--incInt i = i + 1

incInteger :: Integer -> Integer
incInteger i = i + 1


--inc :: Num a => a -> a
inc :: Integral a => a -> a
inc i = i + 1

doubleAdd :: Int -> Int -> Int
doubleAdd n m = 2 * n + m

isZero :: Int -> Bool
--isZero i = i == 0
isZero 0 = True
isZero n = False


-- (eloszor) :t ifZero ifZero :: (Eq a1, Num a1, Num a2) => a1 -> a2
ifZero :: Int -> Integer
ifZero 0 = 10
ifZero _ = -10

divides :: Int-> Int -> Bool
divides n m = isZero(mod m n)

divisors :: Int -> [Int]
divisors n = [x | x <- [1..n], x `divides` n]

isPrime :: Int -> Bool
isPrime n = divisors n == [1,n]

primesB :: Int -> [Int]
primesB n = [i | i <- [1..n], isPrime i]
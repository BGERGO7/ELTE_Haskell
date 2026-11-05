module Lab7 where

findValueByKey :: Eq a => a -> [(a,b)] -> b
findValueByKey k [] = error "findValueByKey: the given key was not present in the dictionary"
findValueByKey k ((key, value) : xs)
  | k == key  = value
  | otherwise = findValueByKey k xs

allOnes :: [Integer] -> Bool
allOnes [] = True
allOnes (1:xs) = allOnes xs
allOnes _ = False
  
numOfEmpties :: [[a]] -> Integer
numOfEmpties [] = 0
numOfEmpties ([]:xs) = 1 + numOfEmpties xs
numOfEmpties(_:xs) = numOfEmpties xs


myLengthT :: [a] -> Int
myLengthT ls = lengthHelper ls 0
  where
    lengthHelper []     acc = acc
    lengthHelper (x:xs) acc = lengthHelper xs (acc + 1)


myReverseT :: [a] -> [a]
myReverseT ls = reverseHelper ls []
  where
    reverseHelper []     acc = acc
    reverseHelper (x:xs) acc = reverseHelper xs (x:acc)


mySum :: Num a => [a] -> a
mySum ls = mySumHelper ls 0
  where
    mySumHelper [] acc = acc
    mySumHelper (x:xs) acc = mySumHelper xs (x + acc)


-- ????? ezt megnezni otthon!!
myMaximum :: [Integer] -> Integer
myMaximum (x:xs) = maxHelper xs x
  where
    maxHelper [] acc = acc
    maxHelper (x:xs) acc = maxHelper xs (max x acc)



negativeNonNegative :: [Integer] -> ([Integer], [Integer])
negativeNonNegative ls = nnnHelper ls [] [] 
  where
    nnnHelper [] acc1 acc2 = (acc1,acc2)
    nnnHelper (x:xs) acc1 acc2
      | x < 0 = nnnHelper xs (x:acc1) acc2
      | otherwise = nnnHelper xs acc1 (x:acc2)


powersOf2 :: Int -> [Integer]
powersOf2 n = [2^i | i <- [0..n]]


oddEven :: Int -> [Bool]
oddEven n = [even i| i <- [1..n]]


myReplicate :: Int -> a -> [a]
myReplicate n e = [e | _ <- [1..n]]

powerOfTwo :: Integer -> Integer
powerOfTwo n = head[2^i | i <- [0..], 2^i > n]

exponentOf2 :: Integer -> Integer
exponentOf2 n = head[i | i <- [0..], 2^i > n]

divisors :: Integer -> [Integer]
divisors n = [ i | i <- [1..n], n `mod` i == 0]

isPrime :: Integer -> Bool
isPrime n = length (divisors n) == 2

primesToN :: Integer -> [Integer]
primesToN n = [x | x <- [1..n], isPrime x]
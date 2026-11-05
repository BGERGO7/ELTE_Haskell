module Lab5 where
import Data.Char (toUpper)

-- http://lambda.inf.elte.hu/Lists_new.xml

isEmpty :: [a] -> Bool
isEmpty []     = True
isEmpty (x:xs) = False

isSingleton :: [a] -> Bool
isSingleton []     = False
isSingleton (x:[]) = True
isSingleton (x:xs) = False

isDoubleton :: [a] -> Bool
--isDoubleton (x:y:[]) = True
isDoubleton (_:_:[]) = True
isDoubleton _      = False

hd :: [a] -> a
hd (x:_) = x

tl :: [a] -> [a]
tl (_:xs) = xs

toUpperFirst :: String -> String
toUpperFirst "" = ""
toUpperFirst (x:xs) = toUpper x : xs

sum :: Num a => [a] -> a
sum []     = 0
sum (x:xs) = x + Lab5.sum xs

myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs

myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

myMinimum :: [Integer] -> Integer
myMinimum (x:[]) = x
myMinimum (x:xs) = min x (myMinimum xs)

infixl 9 !:!
(!:!) :: [a] -> Int -> a
(x:_) !:! 0 = x
(x:xs) !:! i = xs !:! (i-1)

myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop n (x:xs)
  | n <= 0 = (x:xs)
  | otherwise = myDrop (n-1) xs


myElem :: Eq a => a -> [a] -> Bool
myElem e [] = False
myElem e (x:xs)
  | e == x = True
  | otherwise = myElem e xs

isAscending :: Ord a => [a] -> Bool
isAscending [] = True
isAscending (_:[]) = True
isAscending (x:y:xs)
    | x < y = isAscending (y:xs)
    | otherwise = False
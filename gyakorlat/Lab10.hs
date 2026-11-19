module Lab10 where

import Data.Char 

maybeAdd :: Num a => Maybe a -> Maybe a -> Maybe a
maybeAdd (Just x) (Just y) = Just (x+y)
maybeAdd (Just x) _ = Just x
maybeAdd _ (Just y) = Just y
maybeAdd Nothing Nothing = Nothing

elemIndex :: Eq a => a -> [a] -> Maybe Int
elemIndex e ls = helper e ls 0 where
  helper _ [] _ = Nothing
  helper e (x:xs) i
    | e == x = Just i
    | otherwise = helper e xs (i+1)

maybeSum :: Num a => [Maybe a] -> a
--maybeSum [] = 0
--maybeSum ((Just x ):xs) = x + maybeSum xs
--maybeSum (_:xs) = maybeSum xs 
maybeSum ls = sum [x | (Just x) <- ls]

safePower :: (Num a, Integral b) => a -> b -> Either String a
safePower x i
  | i < 0     = Left "Error case: Negative exponent."
  | otherwise = Right (x ^ i)


-- P :: a -> b -> Pair a b

data Pair a b = P a b deriving (Eq)
instance (Show a, Show b) => Show (Pair a b) where
    show (P x y) = "(" ++ show x ++ ", " ++show y ++ ")"

readInt :: [Char] -> Maybe Int
readInt chs = helper chs 0
  where
    helper [] num = Just num
    helper (x:xs) num
      | isDigit x = helper xs (10 * num + digitToInt x)
      | otherwise = Nothing

-- Feladat: Bináris fák
data BinTree a = Leaf | Node a (BinTree a) (BinTree a) deriving (Show, Eq)

incNodes :: Num a => BinTree a -> BinTree a
incNodes Leaf = Leaf
incNodes (Node x t1 t2) = Node (x + 1) (incNodes t1) (incNodes t2)

-- http://lambda.inf.elte.hu/Conversions.xml

-- fromIntegral :: (Integral a, Num b) => a -> b

watering :: Integer -> Integer
watering n = ceiling ((fromIntegral n) * 0.25 / 1.8)

iSqrt :: Integer -> Integer
iSqrt x = round (sqrt (fromIntegral x))
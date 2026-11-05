module Hazi7 where

import Data.Char (chr, ord)

sumSingletons :: [[Integer]] -> Integer
sumSingletons [] = 0
sumSingletons ([x] : xs) = x + sumSingletons xs
sumSingletons (_ : xs) = sumSingletons xs

longKey :: [(Char, Int)]
longKey = [('h', 1), ('e', 2), ('l', 3), ('o', 4), ('w', 5), ('r', 6), ('d', 7)]

shiftChar :: Char -> [(Char, Int)] -> Char
shiftChar ch [] = ch
shiftChar ch ((x, n) : xs)
  | ch == x = chr (ord ch + n)
  | otherwise = shiftChar ch xs


encrypt :: [(Char, Int)] -> String -> String
encrypt _ [] = []
encrypt k (ch : xs) = shiftChar ch k : encrypt k xs

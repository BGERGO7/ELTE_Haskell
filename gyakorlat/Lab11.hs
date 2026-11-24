module Lab11 where

-- http://lambda.inf.elte.hu/HigherOrderFunctions.xml

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile _ [] = []
myDropWhile b (x:xs)
  | b x = myDropWhile b xs
  | otherwise = (x:xs)


myIterate :: (a -> a) -> a -> [a]  
myIterate f e = e : myIterate f (f e)

fibPairs :: [(Integer, Integer)]
fibPairs = iterate (\(a,b) -> (b, a+b)) (0,1)

myAny :: (a -> Bool) -> [a]{-véges-} -> Bool
myAny _ [] = False
myAny b (x:xs)
  | b x = True
  | otherwise = myAny b xs

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys
myZipWith _ _ _ = []

-- Gyakorlo feladatok

count :: (a -> Bool) -> [a] -> Int
count b l = length $ filter b l         -- length (filter b l)


filters :: Eq a => [a] -> [a] -> [a]
filters l1 l2 = filter (`notElem` l1) l2

applyOnNeighbours :: (a -> a -> a) -> [a] -> [a]
applyOnNeighbours _ [] = []
applyOnNeighbours f l = zipWith f l (tail l)

pascalTriangle :: [[Integer]]
pascalTriangle = iterate nextRow [1] where
    nextRow l = 1 : applyOnNeighbours (+) l ++ [1]


decompress :: Eq a => [(a, Int)] -> [a]
decompress l = concat( map (\(ch, x) -> replicate x ch) l)
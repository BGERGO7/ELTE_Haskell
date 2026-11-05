module Lab8 where

-- Listakifejezés több generátorral http://lambda.inf.elte.hu/ListComprehensions.xml

time :: [(Int, Int)]
time = [ (h,m) | h <- [0..23], m <- [0..59] ]

dominoes :: Integer -> [(Integer, Integer)]
dominoes n = [(x,y) | x <- [0..n], y <- [0..n], x<=y]
--dominoes n = [(x,y) | x <- [0..n], y <- [x..n]]

monotonousSeq :: Int -> [Int]
monotonousSeq i = [x | x <- [1..i], _ <- [1..x]]

indexString :: [Char] -> [(Integer, Char)] -- String -> [(Integer, Char)]
indexString str = zip [1..] str

mountain :: Integer -> [Integer]
mountain n = concat [[1..n], [n-1, n-2 .. 2]]

mountains :: Integer -> [Integer]
mountains n = concat [mountain x | x <- [1..n]]

-- Szövegek összefűzése és darabolása

asteriskSeq :: String
asteriskSeq = unwords [replicate x '*'| x <- [1..]] -- unwords el szokozoket rakunk a csillagok koze!
--asteriskSeq = unwords [['*' | y <- [1..x]]| x <- [1..]]

-- Mintaillesztés listakifejezésben

numOfAs :: String -> Int
numOfAs str = length [1 |'a' <- str ]

countOfAs :: String -> Int
countOfAs str = length [1 | "az" <- words str] -- words: concat ["A", "Haskell", "nyelv"] => "AHaskellnyelv" :: [Char]

distantPairs :: [(Integer,Integer)] -> Int
distantPairs ls = length [1 | (x,y) <- ls, abs (x-y) >= 2]

everyNth :: Integer -> [a] -> [a]
everyNth n ls =  [x | (x,1) <- zip ls (cycle [1..n])]

excludeSquares :: [Integer]
excludeSquares = concat [[n^2+1 .. (n+1)^2-1] | n <- [1..]]
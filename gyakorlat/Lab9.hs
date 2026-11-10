module Lab9 where
import GHC.Float (word2Double)
import Data.Data

-- http://lambda.inf.elte.hu/TypeClasses.xml

-- http://lambda.inf.elte.hu/AlgebraicDataTypes.xml

data Answer = No | Maybe | Yes deriving (Show, Eq, Ord)

toString :: Answer -> String
toString No = "No"
toString Maybe = "Maybe"
toString Yes = "Yes"

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Eq, Show, Enum)

isWeekend :: Day -> Bool
isWeekend Sat = True
isWeekend Sun = True
isWeekend _ = False

tomorrow :: Day -> Day
tomorrow Mon = Tue
tomorrow Tue = Wed
tomorrow Wed = Thu
tomorrow Thu = Fri
tomorrow Fri = Sat
tomorrow Sat = Sun
tomorrow Sun = Mon

betterTomorrow :: Day -> Day
betterTomorrow Sun = Mon
betterTomorrow day = succ day

data USTime = AM Int Int | PM Int Int deriving (Show, Eq)

showUSTime :: USTime -> String
showUSTime (AM h m) = show h ++ "." ++ show m ++ " am"
showUSTime (PM h m) = show h ++ "." ++ show m ++ " pm"

workdays :: [Day] -> [Day]
workdays days = [d | d <- days, not (isWeekend d)]
--workdays [] = []
--workdays (x:xs) 
  --  | isWeekend x == False = x : workdays xs
  --  | otherwise = workdays xs


-- Bas feladat
-- http://lambda.inf.elte.hu/Bas.xml#pontoz%C3%A1s
type DoubleRoll = (Int,Int)

data RollResult
   = Simple Int -- ha különbözőek a számok (és nem a 21 jön ki belőlük)
   | Bas Int    -- ha két egyforma számot dobunk (,,bás'')
   | TwentyOne  -- ha 1-est és 2-est dobunk
     deriving (Eq, Show, Ord, Data, Typeable)

toResult :: DoubleRoll -> RollResult
toResult (1, 2) = TwentyOne 
toResult (2, 1) = TwentyOne 
toResult (x,y)
  | x == y = Bas x
  | otherwise = Simple (max x y * 10 + min x y)

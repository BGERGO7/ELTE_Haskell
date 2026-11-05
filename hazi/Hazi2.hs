majorityFunction :: Bool -> Bool -> Bool -> Bool
majorityFunction True True _ = True
majorityFunction True _ True = True
majorityFunction _ True True = True
majorityFunction _ _ _ = False

replaceMult :: Char -> Char
replaceMult 'x' = '*'
replaceMult c = c
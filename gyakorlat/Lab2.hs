module Lab2 where

inc :: Int -> Int
inc n = n + 1

multAdd n m = n * m + n + m

infixr 2 |:|
(|:|) :: Bool -> Bool -> Bool

(|:|) False False = False
(|:|) _ _ = True

xor :: Bool -> Bool -> Bool
xor True True = False
xor False False = False
xor _ _ = True

infix 1 -->
(-->) :: Bool -> Bool -> Bool
(-->) True False = False
(-->) _ _ = True

isA :: Char -> Bool
isA 'a' = True
isA 'A' = True
isA _ = False

replaceNewline :: Char -> Char
replaceNewline '\n' = ' '
replaceNewline ch = ch

isNewline :: Char -> Bool
isNewline '\n' = True
isNewline ch = False

isVowel :: Char -> Bool

isVowel 'a' = True
isVowel 'e' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel 'u' = True
isVowel x = False

isVowel ch = elem ch ['a', 'e','i','o','u'] == True
isVowel _ = False
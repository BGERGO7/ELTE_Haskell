module Hazi10 where

data Privilege = Unprivileged | Admin deriving (Show, Eq)

data Cookie = LoggedIn String Privilege | LoggedOut deriving (Show, Eq)

db :: [(String, String, Privilege)]
db = [("dumbledore","abracadabra",Unprivileged), ("root", "secret", Admin), ("bela", "korte", Unprivileged)]

register :: String -> String -> Cookie -> [(String, String, Privilege)] -> [(String, String, Privilege)]
register username password (LoggedIn _ Admin) database =
    (username, password, Unprivileged) : database
register _ _ _ database = database

getUser :: String -> [(String, String, Privilege)] -> Maybe (String, Privilege)
getUser _ [] = Nothing
getUser username ((name, password, privilege) : rest)
    | username == name = Just (password, privilege)
    | otherwise = getUser username rest

login :: String -> String -> [(String, String, Privilege)] -> Cookie
login _ _ [] = LoggedOut
login username password ((name, storedPassword, privilege) : rest)
    | username == name && password == storedPassword =
        LoggedIn username privilege
    | otherwise = login username password rest
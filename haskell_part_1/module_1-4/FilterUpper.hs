import Data.Char

delAllUpper :: String -> String
delAllUpper = unwords . filter (any (not . isUpper)) . words
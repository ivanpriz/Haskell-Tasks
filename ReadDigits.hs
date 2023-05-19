import Data.Char

readDigits :: String -> (String, String)
readDigits str = (takeWhile isDigit str, dropWhile isDigit str)


filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj predA predB = filter f where
  f x = predA x || predB x

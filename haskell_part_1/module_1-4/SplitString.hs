charPresentInString :: Char -> String -> Bool
charPresentInString ch "" = False
charPresentInString ch (s:str) = if s == ch then True else charPresentInString ch str


splitString :: Char -> String -> [String]
splitString delimeter str = case charPresentInString delimeter str of
  True -> reverse $ helper [] delimeter str where
          helper res delimeter "" = res
          helper res delimeter string = let
            chunk = takeWhile (\x -> x /= delimeter) string
            remains = dropWhile (\x -> x == delimeter) $ dropWhile (\x -> x /= delimeter) string
            in helper (chunk:res) delimeter remains
  False -> []
--    TODO handle no delimeter case

--splitStringSubstring :: String -> String -> [String]
--splitStringSubstring delimeter str =

strip :: String -> String
strip str = reverse $ dropWhile (\x -> x == ' ') $ reverse $ (dropWhile (\x -> x == ' ') str)
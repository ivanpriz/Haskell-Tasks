import Data.Char(isDigit)

--findDigit :: [Char] -> Maybe Char
--findDigit = helper where
--  helper [] = Nothing
--  helper (c:cs)
--    | isDigit c = Just c
--    | not (isDigit c) && not (null cs) = helper cs
--    | otherwise = Nothing

findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (x:xs) = if isDigit x then Just x else findDigit xs


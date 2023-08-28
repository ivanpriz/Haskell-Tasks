import Data.Char
import Data.Maybe


data Token = Number Int | Plus | Minus | LeftBrace | RightBrace
   deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken [] = Nothing
asToken (x:[])
  | isDigit x = Just (Number (digitToInt x))
  | x == '(' = Just LeftBrace
  | x == ')' = Just RightBrace
  | x == '+' = Just Plus
  | x == '-' = Just Minus
  | otherwise = Nothing
asToken (x:xs) = helper [] (x:xs) where
  helper nums [] = Just $ Number $ (read $ reverse nums :: Int)
  helper nums (d:ds) = if isDigit d then helper (d:nums) ds else Nothing


tokenize :: String -> Maybe [Token]
tokenize input = let res = map asToken (words input) in if any isNothing res then Nothing else Just (map fromJust res)
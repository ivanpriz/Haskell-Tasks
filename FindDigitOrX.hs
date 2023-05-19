import Data.Char(isDigit)

findDigit :: [Char] -> Maybe Char

findDigitOrX :: [Char] -> Char
findDigitOrX str = case findDigit str of
  Just d -> d
  Nothing -> 'X'
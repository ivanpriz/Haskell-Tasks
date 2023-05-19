oddsOnly :: Integral a => [a] -> [a]
oddsOnly lst = reverse (helper [] lst)
  where
    helper odds [] = odds
    helper odds (x:xs) = if odd x then helper (x:odds) xs else helper odds xs
    reverse = reverseHelper []
      where
        reverseHelper acc [] = acc
        reverseHelper acc (x:xs) = reverseHelper (x:acc) xs
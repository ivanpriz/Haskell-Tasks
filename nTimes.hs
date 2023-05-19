nTimes :: a -> Int -> [a]
nTimes val n
  | n == 0 = []
  | otherwise = val : nTimes val (n-1)
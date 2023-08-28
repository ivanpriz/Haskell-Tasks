squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = helper [] where
  helper :: Num a => [a] -> [a] -> [a]
  helper acc [] = reverse acc
  helper acc (x:xs) = helper (x ^ 3 : x ^ 2 : acc) xs
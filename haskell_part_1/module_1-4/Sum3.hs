sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 xs ys zs = sum2 (sum2 xs ys) zs where
  sum2 :: Num a => [a] -> [a] -> [a]
  sum2 [] [] = []
  sum2 (x:xs) [] = x:xs
  sum2 [] (y:ys) = y:ys
  sum2 (x:xs) (y:ys) = (x+y) : sum2 xs ys

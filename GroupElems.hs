groupElems :: Eq a => [a] -> [[a]]
groupElems = reverse . helper [] where
  helper :: Eq a => [[a]] -> [a] -> [[a]]
  helper [] [] = []
  helper [] (x:xs) = helper [[x]] xs
  helper acc [] = acc
  helper ((lastVal:lastVals):remainAcc) (x:xs)
   | x == lastVal = helper ((x:lastVal:lastVals):remainAcc) xs
   | otherwise = helper ([x]:((lastVal:lastVals):remainAcc)) xs
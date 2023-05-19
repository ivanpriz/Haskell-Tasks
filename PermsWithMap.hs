perms :: [a] -> [[a]]
perms = helper [[]] where
  helper :: [[a]] -> [a] -> [[a]]
  helper acc [] = acc
  helper [[]] (x:xs) = helper [[x]] xs
  helper acc (x:xs) = helper (concatMap (insertToAllIndexes x) acc) xs
    where
      insertToAllIndexes :: a -> [a] -> [[a]]
      insertToAllIndexes val lst = helper [] (length lst) val lst
        where
          helper :: [[a]] -> Int -> a -> [a] -> [[a]]
          helper results index val lst
            | index < 0 = results
            | otherwise = helper ((insertToIndex index val lst):results) (index - 1) val lst
              where insertToIndex index val lst = take index lst ++ (val : (drop index lst))

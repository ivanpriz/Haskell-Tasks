import Debug.Trace
import Text.Printf

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x =
  helper (abs x) 0 0
  where
    helper :: Integer -> Integer -> Integer
           -> (Integer, Integer)
    helper currNum currSum currCount
      | currNum `div` 10 == 0 = (newSum, newCount)
      | otherwise = trace
        (printf "curr num: %d, curr sum: %d, new num: %d, new sum: %d\n" currNum currSum newNum newSum)
        (helper newNum newSum newCount)
      where
        newNum = currNum `div` 10
        newSum = currSum + currNum `mod` 10
        newCount = currCount + 1
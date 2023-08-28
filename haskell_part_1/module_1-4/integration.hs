import Debug.Trace

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b =
  (helper f begin end step 0) * sign
  where helper :: (Double -> Double) -> Double -> Double -> Double -> Double -> Double
        helper f start end distance square
          | start >= end = square
          | otherwise = (helper f (start + distance) end distance (square + f start * distance))

        begin = if a <= b then a else b
        end = if a <= b then b else a
        sign = if a <= b then 1 else (-1)
        step = (end - begin) / 1000
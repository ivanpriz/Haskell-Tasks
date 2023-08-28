fibNumLastNum :: Integer -> Integer
fibNumLastNum x = helper 0 1 0 x where
  helper :: Integer -> Integer -> Integer -> Integer -> Integer
  helper prev prev1 iterno num | num == 0 = 0
                               | num == 1 = 1
                               | iterno == num = prev
                               | otherwise = helper prev1 ((prev + prev1) `mod` 10) (iterno + 1) num


main :: IO ()
main = do
    numS <- getLine
    let num = read numS :: Integer
    let fib = fibNumLastNum num
    putStrLn (show fib)
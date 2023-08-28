fibNum :: Integer -> Integer
fibNum x = helper 0 1 0 x where
  helper :: Integer -> Integer -> Integer -> Integer -> Integer
  helper prev prev1 iterno num | num == 0 = 0
                               | num == 1 = 1
                               | iterno == num = prev
                               | otherwise = helper prev1 (prev + prev1) (iterno + 1) num


main :: IO ()
main = do
    numS <- getLine
    let num = read numS :: Integer
    let fib = fibNum num
    putStrLn (show fib)
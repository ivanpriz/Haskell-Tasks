euclidBcd :: Integer -> Integer -> Integer
euclidBcd a b
    | a == 0 = b
    | b == 0 = a
    | a >= b = euclidBcd (a `mod` b) b
    | b >= a = euclidBcd (b `mod` a) a

main :: IO ()
main = do
    numsLine <- getLine
    let (x : y : _) =  map (\x -> read x :: Integer) (words numsLine)
    putStrLn $ show $ euclidBcd x y

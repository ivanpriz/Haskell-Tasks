import Data.Array

--binSearch :: Ord a => Array Int a -> a -> Int
--binSearch arr val = helper arr val (bounds arr) where
--    helper :: Ord a => Array Int a -> a -> (Int, Int) -> Int
--    helper arr val (start, end)
--        | arr ! midIndex == val = midIndex
--        | otherwise = if arr ! midIndex < val then helper arr val (start, midIndex - 1) else helper arr val (midIndex + 1, end)
--        where midIndex = start + (end - start) `div` 2

binSearch :: Ord a => Array Int a -> a -> Int
binSearch arr val = helper arr val (bounds arr) where
    helper arr val (start, end)
        | start > end = error "Not found"
        | arr ! midIndex == val = midIndex
        | otherwise = if arr ! midIndex > val then helper arr val (start, midIndex - 1) else helper arr val (midIndex + 1, end)
        where midIndex = (start + end) `div` 2

main :: IO ()
main = do
    amountStr <- getLine
    let amount = read amountStr :: Int
    nums <- getLine
    let arr = array (0, amount - 1) (zip [0..] ((\x -> read x :: Int) `map` (words nums)))
    val <- getLine
    let v = read val :: Int
    print $ binSearch arr v

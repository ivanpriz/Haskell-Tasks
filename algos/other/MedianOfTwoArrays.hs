import Data.Array

binSearch :: Ord a => Array Int a -> a -> Int
binSearch arr val = helper arr val (bounds arr) where
    helper arr val (start, end)
        | start > end = error "Not found"
        | arr ! midIndex == val = midIndex
        | otherwise = if arr ! midIndex > val then helper arr val (start, midIndex - 1) else helper arr val (midIndex + 1, end)
        where midIndex = (start + end) `div` 2


findMedian arr1 arr2 = let

    helper arr1 arr2 start end
        | start > end = error "Unable to find median"
        | i < 0 = if -- Take all elements from arr2
        | arr1 ! i > arr2 ! (j + 1) = helper arr1 arr2 start (mid - 1)
        | arr1 ! i < arr2 ! (j + 1) = helper arr1 arr2 (mid + 1) end
        | arr1 ! i <= arr2 ! (j + 1) && arr2 ! j <= (arr1 ! (i + 1)) = m  -- We take i elems from frist arr and j elems from 2
        where
            n = snd $ bounds arr1
            m = snd $ bounds arr2 + 1
            mid = (start + end) `div` 2
            mergedMed = (n + m) `div` 2
            left1size = mid
            left2size = mergedMed - mid

            j = mergedMed - mid
import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict ((!), update, IntMap, fromList)

parent :: Int -> Int
parent i = (i - 1) `div` 2

leftChild :: Int -> Int
leftChild i = 2 * i + 1

rightChild :: Int -> Int
rightChild i = 2 * i + 2

swap :: IntMap Int -> Int -> Int -> IntMap Int
swap arr i j = let
    tmp1 = arr ! i
    tmp2 = arr ! j
    setVal val key = Just val
    in update (setVal tmp1) j (update (setVal tmp2) i arr)

-- sifts value down
-- returns updated heap and list of permutations
siftDown :: IntMap Int -> Int -> Int -> (IntMap Int, ([(Int, Int)], Int))
siftDown heap size i = helper heap ([], 0) size i where
    helper :: IntMap Int -> ([(Int, Int)], Int) -> Int -> Int -> (IntMap Int, ([(Int, Int)], Int))
    helper heap (perms, permsNum) size i
        | i == minFinal = (heap, (reverse perms, permsNum))
        | otherwise = helper (swap heap i minFinal) (((i, minFinal):perms), permsNum + 1) size minFinal
        where
            r = rightChild i
            l = leftChild i
            minIL = if (l < size && heap ! l < heap ! i) then l else i
            minIR = if (r < size && heap ! r < heap ! i) then r else i
            minFinal = if (heap ! minIL <= heap ! minIR) then minIL else minIR

combineLists [] l2 = l2
combineLists (x:xs) l2 = combineLists xs (x:l2)

buildHeap :: IntMap Int -> Int -> (IntMap Int, ([(Int, Int)], Int))
buildHeap arr len = helper arr ([], 0) (len `div` 2) where
    helper :: IntMap Int -> ([(Int, Int)], Int) -> Int -> (IntMap Int, ([(Int, Int)], Int))
    helper heap permAcc (-1) = (heap, permAcc)
    helper heap (perms, pNum) currIndex = case siftDown heap len currIndex of
        (newHeap, (iterPerms, iterPermsNum)) -> helper newHeap (combineLists iterPerms perms, pNum + iterPermsNum) (currIndex - 1)

printPair :: (Int, Int) -> IO ()
printPair (a, b) = do
    putStrLn ""
    putStr $ show a
    putChar ' '
    putStr $ show b

main :: IO ()
main = do
    nStr <- getLine
    let n = read nStr :: Int
    numsStr <- getLine
    let nums = fromList $ zip [0..] (map (\x -> read x :: Int) (words numsStr))
    let res = snd $ buildHeap nums n
    putStr $ show $ snd res
    mapM_ printPair (reverse $ fst res)



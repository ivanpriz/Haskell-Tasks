import Control.Monad.ST
import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict ((!))
import Data.STRef
import Control.Monad
import Data.List


whileM :: Monad m => m a -> m Bool -> m ()
whileM action predicate = do
    p <- predicate
    if p then whileM action predicate else return ()

parent :: Int -> Int
parent i = (i - 1) `div` 2

leftChild :: Int -> Int
leftChild i = 2 * i + 1

rightChild :: Int -> Int
rightChild i = 2 * i + 2

-- returns index with max val
compareIxByVals :: Ord a => Int -> Int -> Int -> (IM.IntMap (STRef s a)) -> ST s Ordering
compareIxByVals size a b heap = do
    aVal <- readSTRef $ heap ! a
    bVal <- readSTRef $ heap ! b
    return (if (a < size && aVal < bVal) then LT else GT)

getMinElIx :: Ord a => Int -> [Int] -> (IM.IntMap (STRef s a)) -> Int
getMinElIx size ixs heap = head $ sortBy (\a b -> runST $ compareIxByVals size a b heap) ixs

--siftDown :: ST s [a] -> Int -> ST s [a]
--siftDown heap i = do
--    currIx <- newSTRef i
--    lcIx <- newSTRef $ leftChild i
--    rcIx <- newSTRef $ rightChild i
--    whileM (swapWith)

--buildHeap :: Ord a => [a] -> ST s [a]
--buildHeap l = do
--    l_ <- mapM newSTRef l  -- Create a list of STRefs
--    let l__ = zip [0..] l_  -- [(Int, STRef)]
--    let d = IM.fromList l__  -- IntMap STRef
--    mapM readSTRef (IM.elems d)

createRefsIM :: [a] -> ST s (IM.IntMap (STRef s a))
createRefsIM l = do
    l_ <- mapM newSTRef l  -- Create a list of STRefs
    return $ IM.fromList $ zip [0..] l_

tmpST heap = do
    mapM readSTRef (IM.elems heap)


main :: IO ()
main = do
    nStr <- getLine
    let n = read nStr :: Int
    numsStr <- getLine
    let nums = map (\x -> read x :: Int) (words numsStr)

--    print $ runST $ createRefsIM nums >>= compareIxByVals n 1 2
    print "a"
--    print $ runST $ compareIxByVals heap n 1 2
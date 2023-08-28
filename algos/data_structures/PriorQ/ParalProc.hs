import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict ((!), update, adjust, IntMap, fromList)
import Data.Foldable (foldrM)


data Heap a = Heap (IntMap a) Int Int deriving Show  -- IntMap, currSize, maxSize
data Proc = Proc Int Int deriving Show  -- proc num and proc free time
instance Eq Proc where
    (Proc xNum xFT) == (Proc yNum yFT) = xFT == yFT && xNum == yNum

instance Ord Proc where
    procX@(Proc xNum xFT) <= procY@(Proc yNum yFT) = (xFT < yFT) || (xFT == yFT && xNum <= yNum)
--    compare procX@(Proc xNum xFT) procY@(Proc yNum yFT) =
--        if xFT > yFT || (xFT == yFT) && xNum > yNum then GT else if procX == procY then EQ else LT

parent :: Int -> Int
parent i = (i - 1) `div` 2

leftChild :: Int -> Int
leftChild i = 2 * i + 1

rightChild :: Int -> Int
rightChild i = 2 * i + 2

swap :: Heap Proc -> Int -> Int -> Heap Proc
swap (Heap arr currSize maxSize) i j = let
    tmp1 = arr ! i
    tmp2 = arr ! j
    setVal val key = Just val
    in Heap (update (setVal tmp1) j $ update (setVal tmp2) i arr) currSize maxSize

get :: Heap Proc -> Int -> Proc
get (Heap mapp _ _) i = mapp ! i

upd :: Heap Proc -> Int -> Proc -> Heap Proc
upd (Heap mapp currSize maxSize) i val = Heap (update (\proc -> Just val) i mapp) currSize maxSize

-- sifts value down
-- returns updated heap
siftDown :: Heap Proc -> Int -> Heap Proc
siftDown heap@(Heap mapp currSize maxSize) i
        | i == minFinal = heap
        | otherwise = siftDown (swap heap i minFinal) minFinal
        where
            r = rightChild i
            l = leftChild i
            minIL = if l < currSize && get heap l < get heap i then l else i
            minIR = if r < currSize && get heap r < get heap i then r else i
            minFinal = if get heap minIL < get heap minIR then minIL else minIR

siftUp :: Heap Proc -> Int -> Heap Proc
siftUp heap i
    | i == 0 || get heap par < get heap i = heap
    | otherwise = siftUp (swap heap i par) par
    where par = parent i

changePriority :: Heap Proc -> Int -> Proc -> Heap Proc
changePriority heap@(Heap mapp currSize maxSize) i val
    | oldVal > val = siftUp newHeap i
    | otherwise = siftDown newHeap i
    where
        oldVal = get heap i
        newHeap = upd heap i val

extractMax :: Heap Proc -> (Proc, Heap Proc)
extractMax heap@(Heap mapp currSize maxSize) = (get heap 0, siftDown (Heap newMapp (currSize - 1) maxSize) 0)
    where (Heap newMapp _ _) = upd heap 0 (get heap (currSize-1))

insertH :: Heap Proc -> Proc -> Heap Proc
insertH heap@(Heap mapp currSize maxSize) proc
    | currSize == maxSize = error "Heap full"
    | otherwise = siftUp (Heap newMapp (currSize + 1) maxSize) currSize
    where (Heap newMapp _ _)  = upd heap currSize proc

-- Receives time for a task
-- Gets first free proc
-- Returns proc num and start time (=proc free time)
-- And new heap with updated proc free time (+ task time)
processTask :: Heap Proc -> Int -> (Proc, Heap Proc)
processTask heap taskTime = (firstFreeProc, newHeap) where
    (firstFreeProc@(Proc num freeTime), newHeapTmp) = extractMax heap
    newHeap = insertH newHeapTmp (Proc num (freeTime + taskTime))

processTasks :: Heap Proc -> [Int] -> [Proc]
processTasks heap tasks = helper heap tasks [] where
    helper :: Heap Proc -> [Int] -> [Proc] -> [Proc]
    helper heap [] acc = acc
    helper heap (t:ts) acc = let
            (proc, newHeap) = processTask heap t
        in helper newHeap ts (proc:acc)

main :: IO ()
main = do
    nm <- getLine
    let [n, m] = map (\x -> read x :: Int) (words nm)
    tasksTimesStr <- getLine
    let tasksTimes = map (\x -> read x :: Int) (words tasksTimesStr)
    let procs = map (\(n, ft) -> Proc n ft) (zip [0..] [0 | _ <- [0..n-1]])
    let heap = Heap (fromList $ zip [0..] procs) n n
--    print heap
--
--    let (p1, nh1) = extractMax heap
--    print p1
--    print nh1
--
--    let nh2 = insertH nh1 (Proc 0 1)
--    print nh2
--
--    let (p3, nh3) = extractMax nh2
--    print p3
--    print nh3
--
--    let (p4, nh4) = extractMax nh3
--    print p4
--    print nh4
--    print "Processing t 1 n1"
--    let (p1, nh1) = processTask heap 1
--    print p1
--    print nh1
--
--    print "Processing t 1 n2"
--    let (p2, nh2) = processTask nh1 1
--    print p2
--    print nh2
--
--    print "Processing t 1 n3"
--    let (p3, nh3) = processTask nh2 1
--    print p3
--    print nh3
--
--    print "Processing t 1 n4"
--    let (p4, nh4) = processTask nh3 1
--    print p4
--    print nh4

    let results = processTasks heap tasksTimes
    foldrM (\(Proc n t) acc -> do
            putStr $ show n
            putStr " "
            putStrLn $ show t
        ) () results

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap)
import Data.Foldable (foldrM)

-- Код для кучи
data Heap a = Heap (IntMap a) Int Int deriving Show  -- IntMap, currSize, maxSize
data CharFreq = CharFreq (Maybe Char) Int deriving Show

instance Eq CharFreq where
    (CharFreq _ fx) == (CharFreq _ fy) = fx == fy

instance Ord CharFreq where
    (CharFreq _ fx) <= (CharFreq _ fy) = fx <= fy

parent :: Int -> Int
parent i = (i - 1) `div` 2

leftChild :: Int -> Int
leftChild i = 2 * i + 1

rightChild :: Int -> Int
rightChild i = 2 * i + 2

-- swaps 2 elements in heap
swap :: Heap a -> Int -> Int -> Heap a
swap (Heap arr currSize maxSize) i j = let
    tmp1 = arr IM.! i
    tmp2 = arr IM.! j
    setVal val key = Just val
    in Heap (IM.update (setVal tmp1) j $ IM.update (setVal tmp2) i arr) currSize maxSize

get :: Ord a => Heap a -> Int -> a
get (Heap mapp _ _) i = mapp IM.! i

upd :: Ord a => Heap a -> Int -> a -> Heap a
upd (Heap mapp currSize maxSize) i val = Heap (IM.update (\_ -> Just val) i mapp) currSize maxSize

-- sifts value down, returns updated heap
siftDown :: Ord a => Heap a -> Int -> Heap a
siftDown heap@(Heap mapp currSize maxSize) i
        | i == minFinal = heap
        | otherwise = siftDown (swap heap i minFinal) minFinal
        where
            r = rightChild i
            l = leftChild i
            minIL = if l < currSize && get heap l < get heap i then l else i
            minIR = if r < currSize && get heap r < get heap i then r else i
            minFinal = if get heap minIL < get heap minIR then minIL else minIR

siftUp :: Ord a => Heap a -> Int -> Heap a
siftUp heap i
    | i == 0 || get heap par < get heap i = heap
    | otherwise = siftUp (swap heap i par) par
    where par = parent i

extractMin :: Ord a => Heap a -> (a, Heap a)
extractMin heap@(Heap mapp currSize maxSize) = (get heap 0, siftDown (Heap newMapp (currSize - 1) maxSize) 0)
    where (Heap newMapp _ _) = upd heap 0 (get heap (currSize-1))

insertH :: Ord a => Heap a -> a -> Heap a
insertH heap@(Heap mapp currSize maxSize) proc
    | currSize == maxSize = error "Heap full"
    | otherwise = siftUp (Heap newMapp (currSize + 1) maxSize) currSize
    where newMapp  = IM.insert currSize proc mapp
-- Закончили код для кучи

-- Tree
data Tree a = Leaf a | Tree (Tree a) a (Tree a) deriving Show

instance Eq a => Eq (Tree a) where
    Leaf x == Leaf y = x == y
    Tree _ rootX _ == Tree _ rootY _ = rootX == rootY
    Leaf x == Tree _ root _ = x == root
    Tree _ root _ == Leaf x = root == x

instance Ord a => Ord (Tree a) where
    Leaf x <= Leaf y = x <= y
    Tree _ rootX _ <= Tree _ rootY _ = rootX <= rootY
    Leaf x <= Tree _ root _ = x <= root
    Tree _ root _ <= Leaf x = root <= x

-- Creates a tree from 2 trees with root with freq = sum of their freqs and these trees as subtrees
joinTrees :: Tree CharFreq -> Tree CharFreq -> Tree CharFreq
joinTrees lx@(Leaf (CharFreq _ freqX)) ly@(Leaf (CharFreq _ freqY)) =
    Tree lx (CharFreq Nothing (freqX + freqY)) ly
joinTrees tx@(Tree _ rootX@(CharFreq _ freqX) _) ty@(Tree _ rootY@(CharFreq _ freqY) _) =
    Tree tx (CharFreq Nothing (freqX + freqY)) ty
joinTrees tx@(Tree _ rootX@(CharFreq _ freqX) _) ly@(Leaf (CharFreq _ freqY)) =
    Tree tx (CharFreq Nothing (freqX + freqY)) ly
joinTrees lx@(Leaf (CharFreq _ freqX)) ty@(Tree _ rootY@(CharFreq _ freqY) _) =
    Tree lx (CharFreq Nothing (freqX + freqY)) ty

createCharsTree :: Heap (Tree CharFreq) -> Tree CharFreq
createCharsTree heap = helper heap where  -- (Leaf (CharFreq Nothing -1))
    helper :: Heap (Tree CharFreq) -> Tree CharFreq
    helper heap@(Heap _ currSize _)
        | currSize == 0 = error "Heap should not be empty"
        | currSize == 1 = fst $ extractMin heap
        | otherwise = let
                (treeX, newHeap1) = extractMin heap
                (treeY, newHeap2) = extractMin newHeap1
                newHeap = insertH newHeap2 (joinTrees treeX treeY)
            in helper newHeap

getCharsCodes :: Tree CharFreq -> Map Char (String, Int)
getCharsCodes (Leaf (CharFreq (Just ch) _)) = M.insert ch ("0", 1) (M.empty :: Map Char (String, Int))
getCharsCodes freqTree = helper freqTree "" (M.empty :: Map Char (String, Int)) where
    helper (Leaf (CharFreq Nothing _)) _ _ = error "No char in leaf node"
    helper (Leaf (CharFreq (Just ch) _)) accCode mapping = M.insert ch (accCode, length accCode) mapping
    helper (Tree lc root rc) accCode mapping =
        (helper lc ('0' : accCode) mapping) `M.union` (helper rc ('1' : accCode) mapping)

-- reverses char codes (because when creating we added to beginning)
reverseCharsCodes :: Map Char (String, Int) -> Map Char (String, Int)
reverseCharsCodes charCodes = M.foldrWithKey f (M.empty :: Map Char (String, Int)) charCodes where
    f :: Char -> (String, Int) -> Map Char (String, Int) -> Map Char (String, Int)
    f ch (code, codeLen) accMap = M.insert ch (reverse code, codeLen) accMap

-- Map of char frequencies
getFreqMap :: String -> Map Char Int
getFreqMap str = helper str (M.empty :: Map Char Int) where
    helper :: String -> Map Char Int -> Map Char Int
    helper [] resMap = resMap
    helper (ch:chars) resMap = case (M.lookup ch resMap) of
        (Just n) -> helper chars (M.update (\x -> Just (x + 1)) ch resMap)
        Nothing -> helper chars (M.insert ch 1 resMap)

encodeStr :: Map Char (String, Int) -> String -> (String, Int)
encodeStr codesMap str = helper codesMap str ("", 0) where
    helper codesMap "" acc@(str, l) = (reverse str, l)
    helper codesMap (ch:chs) acc@(str, l) = let
            (code, codeLen) = codesMap M.! ch
        in helper codesMap chs (code ++ str, l + codeLen)

main :: IO ()
main = do
    s <- getLine
    let fm = getFreqMap s
    let uniqChNum = M.size fm
    let emptyHeap = Heap (IM.fromList $ zip [0..] []) 0 uniqChNum :: Heap (Tree CharFreq)
    let heap = M.foldrWithKey (\ch freq heap -> heap `insertH` Leaf (CharFreq (Just ch) freq)) emptyHeap fm
    let tree = createCharsTree heap
    let chCodes = getCharsCodes tree
    let (encS, encL) = encodeStr chCodes s
    putStr $ show uniqChNum
    putChar ' '
    putStrLn $ show encL
    foldrM (\(ch, (code, codeLen)) acc -> do
                putChar ch
                putStr ": "
                putStrLn $ (reverse code)
            ) () (M.toList chCodes)
    putStrLn encS

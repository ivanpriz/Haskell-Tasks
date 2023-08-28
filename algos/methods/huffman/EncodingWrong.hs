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
    (CharFreq _ fx) <= (CharFreq _ fy) = fx <= fyx

parent :: Int -> Int
parent i = (i - 1) `div` 2

leftChild :: Int -> Int
leftChild i = 2 * i + 1

rightChild :: Int -> Int
rightChild i = 2 * i + 2

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

-- sifts value down
-- returns updated heap
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
-- Код для хранения дерева с символами
-- Дерево будем хранить в массиве. 0 - корневая вершина.
-- Если ix mod 2 == 1 или ix = size - 1, это лист (символ)
-- Иначе это пустая вершина
nodeIsLeaf :: Int -> Int -> Bool
nodeIsLeaf ix treeSize = ix == (treeSize - 1) || ix `mod` 2 == 1

getParentsNum ix = (ix + 1) `div` 2

-- Код можно получить = getOnes (кол-во родителей - 1) + ((ix + 1) mod 2)
getCodeByIx :: Int -> Int -> Maybe Int
getCodeByIx ix treeSize
    | treeSize == 1 = Just 0
    | nodeIsLeaf ix treeSize = Just $ (getOnes $ (getParentsNum ix - 1)) + (ix + 1) `mod` 2
    | otherwise = Nothing
    where
        -- Возвращает число из n единиц и 0 на конце
        getOnes :: Int -> Int
        getOnes 0 = 0
        getOnes 1 = 10
        getOnes n = 10 ^ n + (getOnes (n - 1))

-- Получаем на вход кучу с символами и их частотами
getTreeArr :: Heap CharFreq -> IntMap (Maybe Char)
getTreeArr heap@(Heap _ currSize _) = helper heap mapp (2*currSize - 2) where
    -- инциализируем массив-дерево (Сейчас все вершины Nothing)
    -- Идем с конца (currIx = len - 1), т.к. в конце самые низкочастотные символы
    mapp = IM.fromList $ zip [0..] [Nothing | _ <- [0..(2*currSize-2)]]
    helper :: Heap CharFreq -> IntMap (Maybe Char) -> Int  -> IntMap (Maybe Char)
    helper heap@(Heap _ currSize _) mapp currIx
        -- Если в куче остался один или 0 элементов, нам больше не нужно оттуда доставать,
        -- Т.к. корень не представляет символ и его частота не важна
        -- Пустой кучи быть не должно
        | currSize == 0 = error "Empty frequency heap should not exist"
        | currSize == 1 = IM.update (\_ -> Just chX) currIx mapp
        -- Заполняем массив представляющий дерево
        | otherwise = helper newHeap newMapp (currIx - 2)
            where
                (elX@(CharFreq chX freqX), newHeap1) = extractMin heap  -- Достем из x = кучи мин эл, пишем в currIx
                (elY@(CharFreq chY freqY), newHeap2) = extractMin newHeap1  -- Достем из y = кучи мин эл, пишем в currIx - 1
                newHeap = insertH newHeap2 (CharFreq Nothing (freqX + freqY))  -- Кладем в кучу (Nothing, snd x + snd y)
                newMapp = IM.update (\_ -> Just chY) (currIx - 1) (IM.update (\_ -> Just chX) currIx mapp)

-- Заведем мапку из символов в частоты и посчитаем уникальные символы
getFreqMap :: String -> Map Char Int
getFreqMap str = helper str (M.empty :: Map Char Int) where
    helper :: String -> Map Char Int -> Map Char Int
    helper [] resMap = resMap
    helper (ch:chars) resMap = case (M.lookup ch resMap) of
        (Just n) -> helper chars (M.update (\x -> Just (x + 1)) ch resMap)
        Nothing -> helper chars (M.insert ch 1 resMap)

-- ф-я для рассчета кодов из дерева в фолдре для получения мапки кодов
getCodeFromMB :: Int -> IM.Key -> Maybe Char -> Map Char Int -> Map Char Int
getCodeFromMB treeSize ix Nothing acc = acc
getCodeFromMB treeSize ix (Just ch) acc = case getCodeByIx ix treeSize of
    (Just code) -> M.insert ch code acc
    Nothing -> error "Char found in non-leaf node of code tree"

codeLen c = helper c 1 where
    helper c l
        | c < 10 = l
        | otherwise = helper (c `div` 10) (l + 1)

encodeStr :: Map Char Int -> String -> (String, Int)
encodeStr codesMap str = helper codesMap str ("", 0) where
    helper codesMap "" acc = acc
    helper codesMap (ch:chs) acc@(str, l) = let
            code = codesMap M.! ch
        in helper codesMap chs (str ++ show code, l + codeLen code)

main :: IO ()
main = do
    s <- getLine
    let fm = getFreqMap s
    let uniqChNum = M.size fm
    let emptyHeap = Heap (IM.fromList $ zip [0..] []) 0 uniqChNum :: Heap CharFreq
    let heap = M.foldrWithKey (\ch freq heap -> heap `insertH` (CharFreq (Just ch) freq)) emptyHeap fm
    let codeTree = getTreeArr heap
    let codes = IM.foldrWithKey (getCodeFromMB (IM.size codeTree)) (M.empty :: Map Char Int) codeTree
    let (encS, encL) = encodeStr codes s

    putStr $ show uniqChNum
    putChar ' '
    putStrLn $ show encL
    foldrM (\(ch, code) acc -> do
            putChar ch
            putStr ": "
            putStrLn $ show code
        ) () (M.toList codes)
    putStrLn encS

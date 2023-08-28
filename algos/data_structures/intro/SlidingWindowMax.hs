import Debug.Trace

type Stack a = [a]

emptyS :: Stack a
emptyS = []x``

putS :: Stack a -> a -> Stack a
putS stack val = val:stack

popS :: Stack a -> (Stack a, Maybe a)
popS [] = ([], Nothing)
popS (x:xs) = (xs, Just x)

data StackWithMax a = StackWithMax (Stack a) (Stack a) deriving Show

emptySM :: Ord a => StackWithMax a
emptySM = StackWithMax [] []
    
putSM :: Ord a => StackWithMax a -> a -> StackWithMax a
putSM (StackWithMax [] []) newV = StackWithMax [newV] [newV]
putSM (StackWithMax vals (m:ms)) newV = StackWithMax (newV:vals) ((max newV m):m:ms)

popSM :: Ord a => StackWithMax a -> (StackWithMax a, Maybe a)
popSM stack@(StackWithMax [] []) = (stack, Nothing)
popSM (StackWithMax (v:vs) (m:ms)) = (StackWithMax vs ms, Just v)

maxSM :: Ord a => StackWithMax a -> Maybe a
maxSM (StackWithMax [] []) = Nothing
maxSM (StackWithMax (v:vs) (m:ms)) = Just m

data QueueWithMax a = QueueWithMax (StackWithMax a) (StackWithMax a) deriving Show

emptyQM :: Ord a => QueueWithMax a
emptyQM = QueueWithMax (StackWithMax [] []) (StackWithMax [] [])

pushQM :: Ord a => QueueWithMax a -> a -> QueueWithMax a
pushQM (QueueWithMax  ls rs) x = QueueWithMax (putSM ls x) rs

transferQM :: Ord a => QueueWithMax a -> QueueWithMax a
transferQM queue@(QueueWithMax (StackWithMax [] []) rs) = queue
transferQM (QueueWithMax  ls rs) = let (newLs, val) = popSM ls in
    case val of 
        Just v -> transferQM (QueueWithMax newLs (putSM rs v))
        Nothing -> undefined

popQM :: Ord a => QueueWithMax a -> (QueueWithMax a, Maybe a)
popQM queue@(QueueWithMax (StackWithMax [] []) (StackWithMax [] [])) = (queue, Nothing)
popQM queue@(QueueWithMax ls (StackWithMax [] [])) = popQM $ transferQM queue
popQM (QueueWithMax ls rs) = let (newRs, v) = popSM rs in (QueueWithMax ls newRs, v)

maxQM :: Ord a => QueueWithMax a -> Maybe a
maxQM queue@(QueueWithMax (StackWithMax [] []) (StackWithMax [] [])) = Nothing
maxQM queue@(QueueWithMax ls rs) = case (maxSM ls, maxSM rs) of
    (Just x, Just y) -> Just $ max x y
    (Just x, Nothing) -> Just x
    (Nothing, Just y) -> Just y
    (Nothing, Nothing) -> Nothing

processNums :: [Int] -> QueueWithMax Int -> [Maybe Int] -> ([Maybe Int], QueueWithMax Int)
processNums [] q results = (results, q)
processNums (n:nums) q results = let newQ = pushQM (fst $ popQM q) n in processNums nums newQ ((maxQM newQ):results)

fillQ :: [Int] -> QueueWithMax Int -> Int -> (QueueWithMax Int, [Int])
fillQ nums q 0 = (q, nums)
fillQ (n:nums) q amount = fillQ nums (pushQM q n) (amount - 1)

--formOutput :: [Int] -> String -> String
--formOutput [] acc = acc
--formOutput (n:nums) "" = formOutput nums (show n)
--formOutput (n:nums) acc = formOutput nums (acc ++ " " ++ (show n))

p :: Int -> IO ()
p v = do
    putStr $ show v
    putStr " "

main :: IO ()
main = do
    let q = QueueWithMax (StackWithMax [] []) (StackWithMax [] []) :: QueueWithMax Int
    am <- getLine
    numsStr <- getLine
    let nums = map (\x -> read x :: Int) (words numsStr)
    windowSizeStr <- getLine
    let windowSize = read windowSizeStr :: Int
    let (q1, newNums) = fillQ nums q windowSize
    let (results, resQ) = processNums newNums q1 [maxQM q1]
    mapM_ p (map (\x -> case x of {Just v -> v; Nothing -> undefined }) (reverse results))

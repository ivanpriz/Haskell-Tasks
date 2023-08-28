import Debug.Trace

data Queue a = Queue [a] [a] Int deriving Show

empty :: Queue a
empty = Queue [] [] 0

push :: Queue a -> a -> Queue a
push (Queue ls rs s) x = Queue (x:ls) rs (s+1)

transfer :: Queue a -> Queue a
transfer queue@(Queue [] rs s) = queue
transfer (Queue (l:ls) rs s) = transfer (Queue ls (l:rs) s)

pop :: Queue a -> (Queue a, Maybe a)
pop queue@(Queue [] [] s) = (queue, Nothing)
pop (Queue ls (r:rs) s) = ((Queue ls rs (s-1)), Just r)
pop queue@(Queue (l:ls) [] s) = pop $ transfer queue

isEmpty :: Queue a -> Bool
isEmpty (Queue [] [] s) = True
isEmpty _ = False

-- We could not return q as it does not change.
-- However, we may transfer items, so we want to save transfered q
checkFirst :: Queue a -> (Queue a, Maybe a)
checkFirst queue@(Queue [] [] s) = (queue, Nothing)
checkFirst queue@(Queue ls (r:rs) s) = (queue, Just r)
checkFirst queue@(Queue (l:ls) [] s) = checkFirst $ transfer queue

readPackLine :: IO (Int, Int)
readPackLine = do
    arrAndDur <- getLine
    let [arr, dur] = map (\x -> read x :: Int) (words arrAndDur)
    return (arr, dur)

readAllPacks :: [(Int, Int)] -> Int -> IO [(Int, Int)]
readAllPacks packs 0 = do
    return packs
readAllPacks packs packsLeft = do
    pack <- readPackLine
    readAllPacks (pack : packs) (packsLeft - 1)

-- returns new buffer, pack processing start time and new buffer finish time
processPack :: (Int, Int) -> Queue Int -> Int -> Int -> (Queue Int, Int, Int)
processPack pack@(arr, dur) buffer@(Queue ls rs s) maxSize buffFinishTime
    | s < maxSize =
        let packStartTime = (max arr buffFinishTime) in
        ((push buffer (packStartTime + dur)), packStartTime, (packStartTime + dur))
    | s >= maxSize =
        let (q, firstBuffFinishMb) = checkFirst buffer in
        case firstBuffFinishMb of
            Just firstBuffFinish -> if arr < firstBuffFinish
                                    then (buffer, -1, buffFinishTime)  -- trace ("For -1: buffer " ++ show buffer ++ " fst finish time " ++ show firstBuffFinish ++ " arr time " ++ show arr ++ " curBuffSize " ++ show s)
                                    else processPack pack (fst $ pop buffer) maxSize buffFinishTime
            Nothing -> (buffer, -5, buffFinishTime)


processPacks :: [(Int, Int)] -> Queue Int -> Int -> Int -> [Int] -> [Int]
processPacks [] _ _ _ results = results
processPacks (p:packs) buffer maxSize buffFinishTime results =
    let (newBuffer, packStartTime, newBuffFinishTime) = processPack p buffer maxSize buffFinishTime in
    processPacks packs newBuffer maxSize newBuffFinishTime (packStartTime:results)

main :: IO ()
main = do
    sizeAndN <- getLine
    let [maxSize, n] = map (\x -> read x :: Int) (words sizeAndN)
    packs <- readAllPacks [] n
--    let q = (Queue [] [] 0) :: Queue Int
--    putStrLn $ show q
--    let q1 = push q 1
--    putStrLn $ show q1
--    let q2 = push q1 2
--    putStrLn $ show q2
--    let r = pop q2
--    putStrLn $ show $ r

    mapM_ print (reverse $ processPacks (reverse packs) (Queue [] [] 0) maxSize 0 [])

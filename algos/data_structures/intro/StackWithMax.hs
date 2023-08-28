import Debug.Trace

processCommand :: String -> [Int] -> [Int] -> ([Int], [Int], Maybe Int)
processCommand ('p':'o':'p':[]) stack@(st:sts) maxStack@(m:ms) = (sts, ms, Nothing)
processCommand ('p':'u':'s':'h':' ':val) stack@([]) maxStack@([]) =
    let num = (read val :: Int) in (num:stack, num:maxStack, Nothing)
processCommand ('p':'u':'s':'h':' ':val) stack@(st:sts) maxStack@(m:ms) =
    let num = (read val :: Int) in
    (num:st:sts, (max num m):m:ms, Nothing)
processCommand ('m':'a':'x':[]) stack@(st:sts) maxStack@(m:ms) = (stack, maxStack, Just m)
processCommand c _ _ = trace ("Unknown command " ++ c) ([], [], Nothing)

readAllCommands :: [Int] -> [Int] -> [Int] -> Int -> IO [Int]
readAllCommands results stack maxStack 0 = do
    return results
readAllCommands results stack maxStack commandsLeft = do
    com <- getLine
    case processCommand com stack maxStack of
        (st, mst, Nothing) -> readAllCommands results st mst (commandsLeft - 1)
        (st, mst, Just m) -> readAllCommands (m:results) st mst (commandsLeft - 1)

main :: IO ()
main = do
    commandsNumStr <- getLine
    let commandsNum = read commandsNumStr :: Int
    res <- readAllCommands [] [] [] commandsNum
    mapM_ print (reverse res)

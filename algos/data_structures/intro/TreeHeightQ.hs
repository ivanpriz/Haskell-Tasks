import Data.Array

makeAdjList :: [Int] -> Array Int [Int] -> Int -> Array Int [Int]
makeAdjList (node:xs) adjList currIndex =
    if node == -1
    then makeAdjList xs adjList (currIndex+1)
    else makeAdjList xs adjList_ (currIndex+1)
    where adjList_ = (adjList // [(node, (currIndex:(adjList ! node)))])
makeAdjList [] adjList currIndex = adjList

getRoot :: [Int] -> Int -> Int
getRoot (node:xs) currIndex
    | node == -1 = currIndex
    | otherwise = getRoot xs (currIndex+1)

getChildrenOfMany :: [Int] -> Array Int [Int] -> [Int]
getChildrenOfMany nodes adjList = map (\x -> if x == -1 then length adjList - 1 else x) (concat $ map ((!) adjList) nodes)

calcHeight :: Array Int [Int] -> [Int] -> Int -> Int
calcHeight adjList [] currHeight = currHeight
calcHeight adjList nodes@(x:xs) currHeight = calcHeight adjList (getChildrenOfMany nodes adjList) (currHeight + 1)

main :: IO ()
main = do
    nodesNumCh <- getLine
    nodesDataStr <- getLine
    let nodesNum = read nodesNumCh :: Int
    let nodesData = map (\x -> read x :: Int) (words nodesDataStr)
    let adjList = makeAdjList nodesData (array (0, nodesNum - 1) [(i, []) | i <- [0 .. nodesNum - 1]]) 0
    let root = getRoot nodesData 0
    putStrLn $ show $ calcHeight adjList [root] 0

import Data.List


-- takes segments sorted by their right end
-- finds all segments crossing with leftmost
-- returns crossign segments and remaining segments
findLeftCrossSegs :: [(Int, Int)] -> ([(Int, Int)], [(Int, Int)])
findLeftCrossSegs [] = ([], [])
findLeftCrossSegs segments@(s:segs) = helper [] segments (snd s) where
    helper :: [(Int, Int)] -> [(Int, Int)] -> Int -> ([(Int, Int)], [(Int, Int)])
    helper crossSegs [] rightMostPoint = (reverse crossSegs, [])
    helper crossSegs remains@(seg:allSegs) rightMostPoint
        | fst seg <= rightMostPoint = helper (seg:crossSegs) allSegs rightMostPoint
        | otherwise = (reverse crossSegs, remains)


-- splits list of segments (sorted by right end) into crossing segs groups
-- returns common poing for each group and num of common points
findCrossingPoints :: [(Int, Int)] -> ([Int], Int)
findCrossingPoints segments = helper segments ([], 0) where
    helper :: [(Int, Int)] -> ([Int], Int) -> ([Int], Int)
    helper [] acc = acc
    helper segments (points, pointsNum) = let (crossGroup, remains) = findLeftCrossSegs segments in
        case crossGroup of
            cg@(c:cs) -> helper remains (((snd c) : points), (pointsNum + 1))
            [] -> undefined

readSegments :: Int -> IO [(Int, Int)]
readSegments segNum = helper segNum [] where
    helper :: Int -> [(Int, Int)] -> IO [(Int, Int)]
    helper 0 acc = return acc
    helper segNum acc = do
        rawData <- getLine
        let r@[begin, end] = map (\x -> read x :: Int) (words rawData)
        helper (segNum - 1) ((begin, end) : acc)

p :: Int -> IO ()
p v = do
    putStr $ show v
    putStr " "

main :: IO ()
main = do
    segmentsNumStr <- getLine
    let segmentsNum = read segmentsNumStr :: Int
    segments <- readSegments segmentsNum
    let sortedSegments = sortBy (\a b -> if (snd a) >= (snd b) then GT else LT) segments
    let points = findCrossingPoints sortedSegments
    (print . snd) points
    mapM_ p (fst points)

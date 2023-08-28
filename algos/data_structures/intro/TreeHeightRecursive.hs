import Data.Array


calcDistanceFromRoot :: Integer -> Array Integer Integer -> Array Integer Integer -> (Array Integer Integer, Integer)
calcDistanceFromRoot node tree nodesHeightsCache = helper tree nodesHeightsCache node (tree ! node) 1 where
    helper tree nodesHeightsCache node_ parent accHeight
        | parent == -1 = (nodesHeightsCache // [(node_, accHeight)], accHeight)
        | nodesHeightsCache ! parent /= -1 = let newH = accHeight + (nodesHeightsCache ! parent) in (nodesHeightsCache // [(node_, newH)], newH)
        | otherwise = helper tree nodesHeightsCache node_ (tree ! parent) (accHeight + 1)


calc :: Array Integer Integer -> Array Integer Integer -> [Integer] -> Integer -> (Array Integer Integer, Integer)
calc tree caches [] currMax = (caches, currMax)
calc tree caches (x:xs) currMax = let (newCaches, currDist) = (calcDistanceFromRoot x tree caches) in (calc tree newCaches xs (if currMax >= currDist then currMax else currDist))

main :: IO ()
main = do
    nodesNumCh <- getLine
    nodesDataStr <- getLine
    let nodesNum = read nodesNumCh :: Integer
    let nodesData = array (0, nodesNum - 1) (zip [0 .. nodesNum - 1] (map (\x -> read x :: Integer) (words nodesDataStr)))
    let nodesHeightsCache = array (0, nodesNum - 1) ([(i, -1) | i <- [0 .. nodesNum - 1]])
--    putStrLn $ show nodesData
--    putStrLn $ show $ sort $ elems nodesData
--    putStrLn $ show $ calcDistanceFromRoot 0 nodesData nodesHeightsCache
--    putStrLn $ show $ calcDistanceFromRoot 1 nodesData nodesHeightsCache
--    putStrLn $ show $ calcDistanceFromRoot 2 nodesData nodesHeightsCache
--    putStrLn $ show $ calcDistanceFromRoot 3 nodesData nodesHeightsCache
    putStrLn $ show $ snd $ calc nodesData nodesHeightsCache [0 .. nodesNum - 1] (-1)
--    foldr calcDistanceFromRoot nodesHeightsCache nodesData
--    let dist = maximum $ map (calcDistanceFromRoot nodesData) [0 .. nodesNum - 1]
--    putStrLn $ show dist

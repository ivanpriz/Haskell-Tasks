nextPositions :: [[Int]] -> [[[Int]]]
nextPositions x = [[(t:xs)] | xs <- x, t <- [1, 2, 3]]

nextPositionsN :: [[Int]] -> Int -> ([[Int]] -> Bool) -> [[[Int]]]
nextPositionsN b n pred
    | n < 0 = []
    | otherwise = filter pred (helper [b] n) where
        helper boards movesLeft
            | movesLeft > 0 = helper (boards >>= nextPositions) (movesLeft-1)
            | otherwise = boards
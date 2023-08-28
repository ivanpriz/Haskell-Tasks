import Data.List

findSum :: Int -> ([Int], Integer)
findSum n = helper [] 0 1 0 n where
    helper :: [Int] -> Int -> Int -> Int -> Int -> ([Int], Integer)
    helper _ _ _ _ 1 = ([1], 1)
    helper _ _ _ _ 2 = ([2], 2)
    helper nums currSum currNum currAmount desiredSum
        | currSum + currNum * 2 >= desiredSum = ((desiredSum - currSum):nums, (currAmount+1))
        | otherwise = helper (currNum:nums) (currSum + currNum) (currNum + 1) (currAmount + 1) desiredSum

main :: IO ()
main = do
    ns <- getLine
    let n = read ns :: Int
    let (nums, am) = findSum n
    print am
    putStrLn $ intercalate " " $ map show (reverse nums)

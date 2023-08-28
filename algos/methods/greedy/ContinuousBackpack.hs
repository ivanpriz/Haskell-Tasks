import Data.List


-- reads items (cost, weight, marginCost)
readAllItems :: Int -> IO [(Int, Int, Double)]
readAllItems n = helper [] n where
    helper :: [(Int, Int, Double)] -> Int -> IO [(Int, Int, Double)]
    helper acc 0 = return acc
    helper acc n = do
        row <- getLine
        let [cost, weight] = map (\x -> read x :: Int) (words row)
        helper ((cost, weight, (fromIntegral cost / fromIntegral weight)) : acc) (n - 1)

getMarginCost (_, _, mc) = mc
getWeight (_, w, _) = w
getCost (c, _, _) = c

calculateMaxProfit :: [(Int, Int, Double)] -> Int -> Double
calculateMaxProfit items maxWeight = getCost $ helper items maxWeight 0 where
    helper :: [(Int, Int, Double)] -> Int -> Double -> (Double, [(Int, Int, Double)], Int)
    helper [] freeWeight currCost = (currCost, [], freeWeight)
    helper itemsLeft 0 currCost = (currCost, itemsLeft, 0)
    helper (item:itemsLeft) freeWeight currCost =
        let
            weightAdded = min freeWeight (getWeight item)
            costAdded = fromIntegral weightAdded * (getMarginCost item)
        in helper itemsLeft (freeWeight - weightAdded) (currCost + costAdded)

main :: IO ()
main = do
    inpStr <- getLine
    let [n, maxWeight] = map (\x -> read x :: Int) (words inpStr)
    items <- readAllItems n
    let sortedItems = sortBy (\a b -> if getMarginCost a >= getMarginCost b then LT else GT) items
    let profit = calculateMaxProfit sortedItems maxWeight
    print profit

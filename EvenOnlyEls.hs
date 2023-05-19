import Debug.Trace


evenOnly :: Show a => [a] -> [a]
evenOnly = reverse . fst . foldl (\(res, pos) x -> (trace ("x is " ++ show x ++ " res is " ++ show res ++ " pos is " ++ show pos) (if even pos then (x : res, pos + 1) else (res, pos + 1)))) ([], 1)

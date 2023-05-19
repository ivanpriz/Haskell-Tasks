change :: (Ord a, Num a) => a -> [[a]]
change val = [ x | x <- allCombos, sum x == val ]
    where allCombos = helper [] 0 coins
        where
            helper combos 21 vals = combos
            helper combos len vals = helper ((createRepPerms len vals) : combos) (len + 1) vals
            where
              createRepPerms len vals = 1
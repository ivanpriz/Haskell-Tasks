seqA :: Integer -> Integer
seqA n
  | n == 0 = 1
  | n == 1 = 2
  | n == 2 = 3
  | otherwise = let helper :: Integer -> Integer -> Integer -> Integer -> Integer
	  			                          -> Integer
			              helper ak akPlus1 akPlus2 k maxIndex
			                | k == maxIndex = akPlus2
				     			    | otherwise = let akPlus3 = (akPlus2 + akPlus1 - 2 * ak) in
				     		                    helper akPlus1 akPlus2 akPlus3 (k+1) maxIndex
		            in helper 1 2 3 2 n

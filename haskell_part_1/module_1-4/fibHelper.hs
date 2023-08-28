fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | n == (-1) = 1
            | n == (-2) = (-1)
            | n > 1 = helper 0 1 1 n
            | n < 0 = helper 1 (-1) (-2) n
            
helper :: Integer -> Integer -> Integer -> Integer -> Integer
helper x y n numberIndex 	| n == numberIndex = y
				| numberIndex > 0 = helper y (x+y) (n+1) numberIndex
				| numberIndex < 0 = helper y (x-y) (n-1) numberIndex


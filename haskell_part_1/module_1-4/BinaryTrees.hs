data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf x) = 0
height (Node a b) = 1 + max (height a) (height b)

size :: Tree a -> Int
size (Leaf x) = 1
size (Node a b) = 1 + (size a) + (size b)

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf x) = (1, x)
    go (Node x y) = let wentX = go x
                        wentY = go y
                    in (fst wentX + fst wentY, snd wentX + snd wentY)
class StackInterface a where
    empty :: [a]
    empty = []

    push :: [a] -> a -> [a]
    push stack x = x:stack

    pop :: [a] -> ([a], Maybe a)
    pop [] = ([], Nothing)
    pop (x:xs) = (xs, Just x)

instance StackInterface [a]

main :: IO ()
main = do
    let s = []
    let s1 = push s 1
    putStrLn $ show s1
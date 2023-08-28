data Queue a = Queue [a] [a] deriving Show
class QueueInterface q where
    emptyQ :: q a
    pushQ :: q a -> a -> q a
    transfer :: q a -> q a
    popQ :: q a ->  (q a, Maybe a)

instance QueueInterface Queue where
    emptyQ = Queue [] []

    pushQ (Queue ls rs) x = Queue (x:ls) rs

    transfer (Queue [] rs) = Queue [] rs
    transfer (Queue (l:ls) rs) = Queue ls (l:rs)

    popQ (Queue [] []) = ((Queue [] []), Nothing)
    popQ (Queue ls (r:rs)) = ((Queue ls rs), Just r)
    popQ queue@(Queue (l:ls) []) = popQ (transfer queue)

main :: IO ()
main = do
    let s = (emptyQ :: Queue Integer)
    let s1 = pushQ s 1
    putStrLn $ show s1

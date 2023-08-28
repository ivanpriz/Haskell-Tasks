--data Stack a = Stack [a] deriving Show
--
--empty :: Stack a
--empty = Stack []
--
--push :: a -> Stack a -> Stack a
--push x (Stack xs) = Stack (x:xs)
--
--pop :: Stack a -> (Stack a, Maybe a)
--pop (Stack (x:xs)) = (Stack xs, Just x)
--pop (Stack []) = (empty, Nothing)
--
--getAll :: Stack a -> [a]
--getAll (Stack (x:xs)) = x:xs
--getAll (Stack []) = []
--
--pushMany :: [a] -> Stack a -> Stack a
--pushMany [] stack = stack
--pushMany (x:xs) stack = pushMany xs (push x stack)




-- We maintain stack with brackets, current char index (i) and first unclosed bracket index (j)
pushBracket ::  (Maybe [Char], Int, Int) -> Char -> (Maybe [Char], Int, Int)
pushBracket res@(Nothing, _, _) b = res   -- error case
pushBracket ((Just []), i, j) b =
  case b of
    ']' -> (Nothing, i+1, j)
    ')' -> (Nothing, i+1, j)
    '}' -> (Nothing, i+1, j)
    '[' -> (Just ([b]), i+1, i+1)  -- when we put opening bracket on empty stack
    '(' -> (Just ([b]), i+1, i+1)  -- this is first unclosed b now
    '{' -> (Just ([b]), i+1, i+1)
    _ -> (Just [], i+1, j)
pushBracket ((Just stack@(c:cs)), i, j) b =
    case b of
        ']' -> if c == '[' then
            case cs of
                [] -> (Just cs, i+1, -1)  -- if stack became empty, no brackets opened -> first unclosed is -1
                (t:ts) -> (Just cs, i+1, j)  -- if not, than keep it as is
            else (Nothing, i+1, j)  -- actual check cases
        ')' -> if c == '(' then
            case cs of
                [] -> (Just cs, i+1, -1)  -- if stack became empty, no brackets opened -> first unclosed is -1
                (t:ts) -> (Just cs, i+1, j)  -- if not, than keep it as is
            else (Nothing, i+1, j)  -- actual check cases  -- if brackets matches, remove them
        '}' -> if c == '{' then
            case cs of
                [] -> (Just cs, i+1, -1)  -- if stack became empty, no brackets opened -> first unclosed is -1
                (t:ts) -> (Just cs, i+1, j)  -- if not, than keep it as is
            else (Nothing, i+1, j)  -- actual check cases else (Nothing, i+1, j)  -- if not, error case
        '[' -> (Just (b:stack), i+1, j)
        '(' -> (Just (b:stack), i+1, j)
        '{' -> (Just (b:stack), i+1, j)
        _ -> (Just stack, i+1, j)


main :: IO ()
main = do
    chars <- getLine
    let res = foldl pushBracket (Just "", 0, 0) chars
    case res of
        (Nothing, i, j) -> putStrLn $ show i
        (Just [], _, _) -> putStrLn "Success"
        (Just (x:xs), i, j) -> putStrLn $ show j

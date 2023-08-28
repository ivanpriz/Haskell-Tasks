data Log a = Log [String] a deriving Show

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger func msg = \x -> Log [msg] (func x)

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers val func1 func2 = case func1 val of
  (Log [msg1] val1) -> case func2 val1 of (Log [msg2] val2) -> Log [msg1, msg2] val2

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log messages val) func = case func val of (Log messages2 val2) -> Log (messages ++ messages2) val2
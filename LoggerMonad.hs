import Control.Monad (ap, liftM)

data Log a = Log [String] a deriving Show

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger func msg = \x -> Log [msg] (func x)

returnLog :: a -> Log a
returnLog = Log []

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log messages val) func = case func val of (Log messages2 val2) -> Log (messages ++ messages2) val2

instance Functor Log where
  fmap = liftM

instance Applicative Log where
  pure = return
  (<*>) = ap

instance Monad Log where
  return = returnLog
  (>>=) = bindLog

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList val loggers = foldl (\val func -> val >>= func) (return val) loggers
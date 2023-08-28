import Control.Monad.Writer

evalWriter :: Writer w a -> a
evalWriter = fst . runWriter
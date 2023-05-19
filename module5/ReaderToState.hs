import Control.Monad.State
import Control.Monad.Reader

calculateContentLength :: Reader String Int
calculateContentLength = do
    content <- ask           -- this seems to be the same as 'reader id'
    return (length content);

--readerToState :: Reader r a -> State r a
readerToState m = \e -> runReader m e
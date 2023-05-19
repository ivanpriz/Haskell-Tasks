import Control.Monad (ap, liftM)

data Reader r a = Reader { runReader :: (r -> a) }

instance Functor Reader where
  fmap = liftM

instance Applicative Reader where
  pure = return
  (<*>) = ap

instance Monad (Reader r) where
  return x = Reader $ \_ -> x
  m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r

-- This local allows us to run functions on env of other type than the given one
local' :: (r -> r') -> Reader r' a -> Reader r a
local' f m = Reader $ \e -> runReader m (f e)

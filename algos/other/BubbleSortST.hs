import Control.Monad.ST
import Data.IntMap.Strict
import Data.STRef (newSTRef, readSTRef, writeSTRef, modifySTRef)
import Control.Monad


until_ :: Monad m => m a -> m Bool -> m ()
until_ action predicate = do
    action
    b <- predicate
    if b then until_ action predicate else return ()


pa :: IO Bool
pa = getLine >>= (\l -> if l == "exit" then return False else return True)

bsort :: Ord a => [a] -> ST s [a]
bsort l = do
    l_ <- mapM newSTRef l
    let l__ = zip [0..] l_
    let d = fromList l__
    refLast <- newSTRef $ length l - 2
    refIsSwapped <- newSTRef $ False
    until_ (a refIsSwapped refLast d) (p refIsSwapped refLast)
    mapM readSTRef (elems d)
        where
            a refIsSwapped refLast d = do
                lst <- readSTRef refLast
                writeSTRef refIsSwapped False
                forM_ [0..lst] $ \i -> do
                    let e1Ref = findWithDefault (error "no index") i d
                    let e2Ref = findWithDefault (error "no index") (i+1) d
                    e1 <- readSTRef e1Ref
                    e2 <- readSTRef e2Ref
                    when (e1 > e2) $ swap e1Ref e2Ref e1 e2 refIsSwapped
                modifySTRef refLast (flip (-) $ 1)
            p refIsSwapped refLast = do
                sw <- readSTRef refIsSwapped
                last_ <- readSTRef refLast
                if sw == False || last_ == -1 then return False else return True
            swap e1Ref e2Ref e1 e2 refIsSwapped = do
                writeSTRef e1Ref e2
                writeSTRef e2Ref e1
                writeSTRef refIsSwapped True

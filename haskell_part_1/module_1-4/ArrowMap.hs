import Prelude hiding (lookup)

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v

newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

instance MapLike ArrowMap where
  empty = ArrowMap (\x -> Nothing)
  lookup key (ArrowMap func) = func key
  insert key value (ArrowMap func) = let helper = \key value func x -> if x == key then Just value else func x in ArrowMap (helper key value func)
  delete key (ArrowMap func) = let helper = \key func x -> if x == key then Nothing else func x in ArrowMap (helper key func)
  fromList [] = empty
  fromList ((key, val):keyvals) = insert key val (fromList keyvals)

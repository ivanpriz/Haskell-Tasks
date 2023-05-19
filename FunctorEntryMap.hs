data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show

instance Functor (Entry keyType1 keyType2) where
  fmap func (Entry (key1, key2) val) = Entry (key1, key2) (func val)

instance Functor (Map keyType1 keyType2) where
  fmap func (Map entries) = Map (map (fmap func) entries)
import Prelude hiding (lookup)
import qualified Data.List as L

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)

instance MapLike ListMap where
    empty = ListMap []
    lookup k (ListMap []) = Nothing
    lookup k (ListMap (entry:entries)) = if fst entry == k then Just $ snd entry else lookup k (ListMap entries)
    insert k v mapping = helper k v [] mapping where
      helper :: Eq k => k -> v -> [(k,v)] -> (ListMap k v) -> (ListMap k v)
      helper key val entriesBefore (ListMap []) = ListMap ((key, val):(reverse entriesBefore))
      helper key val entriesBefore (ListMap (entry:entries)) =
        if fst entry == key
        then ListMap ((reverse entriesBefore) ++ ((key,val):entries))
        else helper key val (entry:entriesBefore) (ListMap entries)
    delete k mapping = helper k [] mapping where
      helper key entriesBefore (ListMap []) = ListMap $ reverse entriesBefore
      helper key entriesBefore (ListMap (entry:entries))
        | key == fst entry = ListMap $ (reverse entriesBefore) ++ entries
        | otherwise = helper key (entry:entriesBefore) (ListMap entries)
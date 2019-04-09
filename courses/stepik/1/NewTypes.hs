import qualified Data.List as L
import           Prelude   hiding (lookup)

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList []         = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)

-- my "gonna do all by hands" wariant, doh
instance MapLike ListMap where
    empty = ListMap []

    lookup _ (ListMap []) = Nothing
    lookup key (ListMap ((k,v):kvs))
        | key == k = Just v
        | otherwise = lookup key (ListMap kvs)

    insert key val (ListMap l) = ListMap $ insList l where
        insList [] = [(key,val)]
        insList (kv:kvs) = if key == fst kv then (key, val):kvs else kv : insList kvs

    delete key (ListMap l) = ListMap $ delInList l where
        delInList []       = []
        delInList (kv:kvs) = if key == fst kv then kvs else kv : delInList kvs

-- not my smarter one
-- instance MapLike ListMap where
--     empty = ListMap []
--     lookup key (ListMap list) = L.lookup key list
--     insert key value map  = ListMap $ (key, value) : (getListMap $ delete key map)
--     delete key (ListMap list)  = ListMap $ filter (\ (x, _) -> x /= key) list



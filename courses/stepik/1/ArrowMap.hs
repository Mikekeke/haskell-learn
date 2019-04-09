-- import qualified Data.List as L
-- import           Prelude   hiding (lookup)

import Debug.Trace

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v

newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

-- didn't solve myself, idea was the same, but for whatewer (synthax) reason it didnt work
instance MapLike ArrowMap where
    empty = ArrowMap $ const Nothing
    lookup key (ArrowMap f) = f key
    insert key value (ArrowMap f) = ArrowMap (\k -> if k == key then Just value else f k)
    delete key (ArrowMap f) = ArrowMap (\k -> if k == key then Nothing else f k)

    fromList []         = empty
    fromList ((k,v):xs) = insert k v (fromList xs)
    
-- stuff version
-- instance MapLike ArrowMap where
--     empty = ArrowMap $ const Nothing
--     lookup k (ArrowMap f) = f k
--     insert k v (ArrowMap f) = ArrowMap $ \x -> if x == k then return v else f x
--     delete k (ArrowMap f) = ArrowMap $ \x -> if x == k then Nothing else f x
--     fromList = foldr (uncurry insert) empty
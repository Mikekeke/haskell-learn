import qualified Data.List as L
import           Prelude   hiding (lookup)

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v

newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

instance MapLike ArrowMap where
    empty = ArrowMap $ \_-> Nothing


    fromList []         = empty
    fromList ((k,v):xs) = insert k v (fromList xs)


f = \x -> case x of {42 -> Just "ANSWER"; 0 -> Just "ZERO"; _ -> Nothing}

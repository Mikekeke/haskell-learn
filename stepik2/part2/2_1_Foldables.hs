import           Data.Monoid

data Triple a = Tr a a a  deriving (Eq,Show)

instance Foldable Triple where
    foldl f b (Tr x y z) = f(f(f b x)y)z
    foldr f b (Tr x y z) = f x (f y (f z b))
    --some more variants
    -- foldr f ini (Tr x y z) = foldr f ini [x, y, z]
    -- foldl f ini (Tr x y z) = foldl f ini [x, y, z]
    -- foldl f s (Tr x y z) = s `f` x `f` y `f` z


-- instance Foldable Triple where 
--     foldMap f (Tr x y z) = f x <> f y <> f z
    -- info
    -- foldMap :: Monoid m => (a -> m) -> t a -> m
    -- https://en.wikibooks.org/wiki/Haskell/Foldable

    
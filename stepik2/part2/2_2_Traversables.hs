import Control.Applicative
traverse2list :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f [b]
-- traverse2list f = foldr (\x b -> (:) <$> f x <*> b) (pure [])
traverse2list f = foldr (liftA2 (:) . f) (pure [])

-- from solutions
-- import           Data.Foldable
-- traverse2list :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f [b]
-- traverse2list = (. toList) . traverse

data Triple a = Tr a a a  deriving (Eq,Show)

instance Functor Triple where
    fmap f (Tr x y z) = Tr (f x) (f y) (f z)

instance Applicative Triple where
    pure a = Tr a a a
    (Tr f g h) <*> (Tr x y z) = Tr (f x) (g y) (h z)

instance Foldable Triple where
    foldr f ini (Tr x y z) = f x . f y . f z $  ini
    -- foldMap f (Tr x y z) = f x <> f y <> f z

instance Traversable Triple where
    traverse f (Tr x y z) = Tr <$> (f x) <*> (f y) <*> (f z)
    sequenceA (Tr fx fy fz)= Tr <$> fx <*> fy <*> fz

    -- GHCi> foldl (++) "!!" (Tr "ab" "cd" "efg")
    -- "!!abcdefg"
    -- GHCi> traverse (\x -> if x>10 then Right x else Left x) (Tr 12 14 16)
    -- Right (Tr 12 14 16)
    -- GHCi> traverse (\x -> if x>10 then Right x else Left x) (Tr 12 8 4)
    -- Left 8
    -- !!!
    -- GHCi> sequenceA (Tr (Tr 1 2 3) (Tr 4 5 6) (Tr 7 8 9))
    -- Tr (Tr 1 4 7) (Tr 2 5 8) (Tr 3 6 9)

-- =======================================================================
data Result a = Ok a | Error String deriving (Eq,Show)

instance Functor Result where
    fmap f (Ok a) = Ok $ f a
    fmap _ (Error s) = Error s

instance Applicative Result where
    pure = Ok
    (Ok f) <*> (Ok a) = Ok $ f a
    (Error s) <*> _ = Error s
    _ <*> (Error s) = Error s

instance Foldable Result where
    foldr f ini (Ok a) = f a ini
    foldr _ ini _ = ini

    -- from solutions
    foldMap f (Ok x)    = f x
    foldMap _ (Error _) = mempty

instance Traversable Result where
    traverse f (Ok a) = Ok <$> (f a)
    traverse _ (Error s) = Error <$> (pure s)
    -- stepik team
    -- traverse _ (Error e) = pure $ Error e

    sequenceA (Ok fa) = Ok <$> fa
    sequenceA (Error s) = Error <$> (pure s)
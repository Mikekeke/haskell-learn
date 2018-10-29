{-# LANGUAGE TypeOperators #-}

import           Control.Applicative
import           Data.Monoid

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
    fmap f (Ok a)    = Ok $ f a
    fmap _ (Error s) = Error s

instance Applicative Result where
    pure = Ok
    (Ok f) <*> (Ok a) = Ok $ f a
    (Error s) <*> _ = Error s
    _ <*> (Error s) = Error s

instance Foldable Result where
    foldr f ini (Ok a) = f a ini
    foldr _ ini _      = ini

    -- from solutions
    foldMap f (Ok x)    = f x
    foldMap _ (Error _) = mempty

instance Traversable Result where
    traverse f (Ok a)    = Ok <$> (f a)
    traverse _ (Error s) = Error <$> (pure s)
    -- stepik team
    -- traverse _ (Error e) = pure $ Error e

    sequenceA (Ok cont) = Ok <$> cont
    sequenceA (Error s) = pure (Error s)


-- =====================================================
data Tree a = Nil | Branch (Tree a) a (Tree a)  deriving (Eq, Show)

instance Functor Tree where
    fmap _ Nil            = Nil
    fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)

instance Applicative Tree where
    pure x = Branch Nil x Nil
    (Branch la f ra) <*> (Branch l x r) = Branch (la <*> l) (f x) (ra <*>r)
    _ <*> _ = Nil

instance Foldable Tree where
    foldMap _ Nil            = mempty
    foldMap f (Branch l x r) = (foldMap f l) <> f x <> (foldMap f r)

instance Traversable Tree where
    sequenceA Nil = pure Nil
    sequenceA (Branch l cont r) = Branch <$> sequenceA l <*> cont <*> sequenceA r

-- ==========================================================
infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show)

instance (Functor f, Functor g) => Functor (f |.| g) where
    --  fmap :: (a -> b) -> (f |.| g) a -> (f |.| g) b
    --  fmap :: (a -> b) -> (|.|) f g a -> (|.|) f g b
     fmap fn (Cmps x) = Cmps $ fmap (fmap fn) x
     -- shorter
    --  fmap fn cmps = Cmps $ (fmap.fmap) fn (getCmps cmps)

instance (Applicative f, Applicative g) => Applicative (f |.| g) where
    pure = Cmps . pure . pure
    (Cmps fn) <*> (Cmps x) = Cmps $ fmap (<*>) fn <*> x

instance (Foldable f, Foldable g) => Foldable (f |.| g) where
    foldMap fn = foldMap (foldMap fn) . getCmps
    -- or
    -- foldr f ini = foldr (\ga b -> foldr f b ga) ini . getCmps
    -- shorter
    foldr f ini = foldr (flip $ foldr f) ini . getCmps

instance (Traversable f, Traversable g) => Traversable (f |.| g) where
    traverse f (Cmps fga) = Cmps <$> traverse (traverse f) fga

    -- from solutions
    -- sequenceA (Cmps fgapl) = Cmps <$> sequenceA (sequenceA <$> fgapl)
    -- sequenceA = fmap Cmps . (traverse sequenceA . getCmps)
    -- traverse p = fmap Cmps . (traverse . traverse) p . getCmps


-- some my tests
seqA :: Applicative f => [f a] -> f [a]
seqA [] = pure []
seqA (x:xs) = (:) <$> x <*> seqA xs

ap' :: [[a] -> [a]] -> [[a]] -> [[a]]
ap' l1 l2 = [f x | f <- l1, x <- l2]
seqList :: [[a]]-> [[a]]
-- not! seqList [] = [] - coz effect of empty list, any seqList will return []
seqList [] = [[]]
seqList (x:xs) = ((:) <$> x) `ap'` seqList xs
import Data.Function
import Data.Monoid

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

cnt1 = Un 42
cnt3 = Bi 1 2 cnt1
cnt5 = Bi 3 4 cnt3

instance Functor OddC where
    fmap f (Un a) = Un $ f a
    fmap f (Bi a1 a2 o) = on Bi f a1 a2 (fmap f o)

instance Foldable OddC where
    foldr f x (Un a) = f a x
    foldr f x (Bi a1 a2 o) = f a1 (f a2 (foldr f x o))

    foldMap f (Un a) = f a
    foldMap f (Bi a1 a2 o) = f a1 <> f a2 <> foldMap f o

instance Traversable OddC where
    traverse fa (Un a) = Un <$> fa a
    traverse fa (Bi a1 a2 o) = Bi <$> fa a1 <*> fa a2 <*> traverse fa o

    sequenceA (Un a) = fmap Un a
    sequenceA (Bi a1 a2 o) = Bi <$> a1 <*> a2 <*> sequenceA o

import Data.Fix

data ListF a f = Nil | Cons a f

instance Functor (ListF a) where
    fmap _ Nil = Nil
    fmap f (Cons a fk) = Cons a (f fk)

init
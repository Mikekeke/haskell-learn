import Data.Monoid
import Data.Foldable
data HeapTree a = Nil | Sub (HeapTree a) a (HeapTree a) 

empty :: Ord a => HeapTree a
empty = Nil

instance Show a => Show (HeapTree a) where
    show Nil = ""
    show (Sub l v r) = "[" ++ show l ++ show v ++ show r ++ "]"

insert :: Ord a => a ->  HeapTree a -> HeapTree a
insert a Nil = Sub Nil a Nil
insert a s@(Sub _ v _) | a >= v = Sub s a Nil
insert a (Sub l v Nil) = (Sub l v (Sub Nil a Nil))
insert a (Sub Nil v r) = (Sub (Sub Nil a Nil) v r)
insert a (Sub l v r) = Sub (insert a r) v l

fromList :: Ord a => [a] -> HeapTree a
fromList l = foldl' (flip insert) empty l
{- =(
λ: fromList [1..10]
[[[[[[[[[[1]2]3]4]5]6]7]8]9]10]
-}

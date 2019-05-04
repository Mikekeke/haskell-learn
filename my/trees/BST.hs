data BST a = Nil | Cons (BST a) a (BST a) deriving Show

instance Foldable BST where
    foldr _ b Nil = b
    foldr f b (Cons l x r) = foldr f (f x (foldr f b r)) l

insert :: (Ord a, Eq a) => a -> BST a -> BST a
insert a Nil = Cons Nil a Nil
insert a (Cons l v r) = case a < v of
    True -> Cons (insert a l) v r
    False -> Cons l v (insert a r)

delete :: (Ord a, Eq a) => a -> BST a -> BST a
delete a Nil = Nil
delete a t@(Cons l x r) | a /= x = if a < x then Cons (delete a l) x r else Cons l x (delete a r)
                      | otherwise = deleteFound t

deleteFound (Cons Nil x Nil)           = Nil
deleteFound (Cons l x Nil)             = l
deleteFound (Cons Nil x r)             = r
deleteFound t@(Cons l x r) = Cons l sc (delete sc r)
    where sc = getSucc t

getSucc (Cons _ _ r) =  go r where
            go (Cons Nil x _) = x
            go (Cons l _ _)   = go l

empty :: BST a
empty = Nil
getRoot Nil = Nothing
getRoot (Cons _ x _) = Just x

fromList :: (Ord a, Eq a) =>  [a] -> BST a
fromList l = go l Nil where
    go [] t = t
    go (x:xs) t = go xs (insert x t)

tstList = [4,2,7,8,1,3]
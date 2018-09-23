insert :: Ord a =>  a -> [a] -> [a]
insert a [] = [a]
insert a (x:xs) = if a <= x then a:x:xs else x : insert a xs 
insSort :: Ord a => [a] -> [a]
insSort [] = []
insSort (x:xs) = insert x (insSort xs)
{-
insSort [3,1,2]
~ insert 3 (insSort [1,2])
~ insert 3 (insert 1 (insSort [2]))
~ insert 3 (insert 1 (insert 2 (insSort []))))
~ insert 3 (insert 1 (insert 2 [])))
~ insert 3 (insert 1 ([2]))
~ insert 3 [1,2]
~ 1 : insert 3 [2]
~ 1 : 2 : insert 3 []
~ 1 : 2 : [3]


-}

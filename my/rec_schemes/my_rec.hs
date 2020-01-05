import Test.QuickCheck

map1 f [] = []
map1 f (x:xs) = f x : map1 f xs

foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)
map2 f = foldr' (\x -> (f x :)) []

listCata f z = cata where
    cata [] = z
    cata (x:xs) = f x (cata xs)

listCata2 f z [] = z
listCata2 f z (x:xs) = f x (listCata2 f z xs)



maps = [map1, map2]
test' f ls = let ls' = (fmap sequence . sequence $ maps) f ls in all (head ls'==) ls'
test = quickCheck (test' (*3) :: [Int] -> Bool)

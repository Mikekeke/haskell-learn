
--permutations
perms' :: [a] -> [[a]]
perms' [] = [[]]
perms' [x] = [[x]]
perms' (x:xs) = concatMap (insertElem x) (perms' xs) where
			insertElem x [] = [[x]]
			insertElem x yss@(y:ys) = (x:yss) : map (y:) (insertElem x ys)


groupElems3 :: Eq a => [a] -> [[a]]
groupElems3 [] = []
groupElems3 [x] = [[x]]
groupElems3 (x:xs)
 | x == head xs =
    let
        (r:rs) = groupElems3 xs
    in
        (x : r) : rs
 | otherwise = [x] : groupElems3 xs


--without '~' not gonna work on infinite lists
evenOnly' :: [a] -> [a]
evenOnly' = snd . foldr (\a ~(xs, ys) -> (a : ys, xs)) ([], [])
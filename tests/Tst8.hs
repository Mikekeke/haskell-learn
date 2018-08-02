group' :: Eq a => [a] -> [[a]]
group' [] = []
group' [x] = [[x]]
group' (x:xs) | x == head xs = let (x':xs') = group' xs in (x : x') : xs'
              | otherwise = [x] : group' xs

--   1:2:[]
--   [1,2]
--   λ: 1:2:[3]
--   [1,2,3]


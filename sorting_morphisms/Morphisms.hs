type ListCata x u = (u, x -> u -> u)

listCata :: ListCata x u -> [x] -> u
listCata (a,f) = cata where
    cata [] = a
    cata (x:xs) = f x (cata xs)

cataProd = listCata (1, (*))
cataRev = listCata ([], \x u -> u ++ [x])
-- ↓↓↓ bad perfomance, lol
cataRev' [] = []
cataRev' l = listCata ([head l], \x u -> x:u) (cataRev' $ tail l)

-- from SO
rev xs = foldr (\x k -> \acc -> k (x:acc)) id xs []
cataRev2 l = listCata (id, \x k acc -> k (x:acc)) l []

type ListAna u x = u -> Maybe (x,u)

listAna :: ListAna u x -> u -> [x] 
listAna f = ana where
    ana u = case f u of
        Nothing -> []
        Just (x,u1) -> x : ana u1

destructCount 0 = Nothing
destructCount n = Just (n , n-1)
count = listAna destructCount

isPrime k = null [ x | x <- [2..k - 1], k `mod` x  == 0]
primeGen 0 = Nothing
primeGen n | isPrime n = Just (n, n-1)
           | otherwise = primeGen (n - 1)
anaPrime = reverse . listAna primeGen

fac = cataProd . count


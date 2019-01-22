go _ [] = (Nothing, [])
go n (x:xs) | n == 0 = (Just x, xs)
            | otherwise = let (result, rest) = go (n-1) xs in (result, x:rest)

run n l = case go n l of 
    (Nothing, l') -> l'
    (Just x, rest) -> x : rest

square x c = c (x^2)

add a b c = c (a+b)

prod1 :: (Num a, Eq a) => [a] -> a
prod1 [] = 0
prod1 (0:_) = 0
prod1 (x:xs) = x * prod1 xs

prod2 :: (Num a, Eq a) => [a] -> ([a] -> a ) -> a
prod2 [] f = 0
prod2 (0:_) f = undefined
prod2 (x:xs) f = x * prod1 xs
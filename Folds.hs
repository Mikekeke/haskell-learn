lengthList :: [a] -> Int
lengthList = foldr (\_ acc -> acc + 1)(0)

lengthList':: [a] -> Int
lengthList'= foldr f (0) where
  f _ acc = acc + 1

lengthList'' :: [a] -> Int
lengthList'' = foldr (const (+1)) 0


sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if odd x then x + s else s) 0

sumOdd' :: [Integer] -> Integer
sumOdd' = foldr f 0 where
  f x s | odd x     = x + s
        | otherwise = s

sumOdd'' :: [Integer] -> Integer
sumOdd'' = foldr (+) 0 . filter odd

filter (\x -> foldr (-) x [2,1,5] == foldl (-) x [2,1,5]) [0..10000]

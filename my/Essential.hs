module Essential where


import           Data.Function

-- & == flip($)
slice :: Int -> [a] -> [[a]]
slice n l = splitAt n l & \(a, b) -> a : slice n b

-- b not in scope for "slice n b"
-- slice2 n l = ((\(a, b) -> a)(splitAt n l)) : slice n b
tt :: Int -> [a] -> [a]
tt n l = fst (splitAt n l)

-- O_o
ttPoitfree = (fst .) . splitAt


res1 :: [Integer]
res1 = foldr (\x y-> map (+x) y) [0, 10] [1..5]
-- same
res2 :: [Integer]
res2 = foldr (\x-> map (+x)) [0, 10] [1..5]
keepInMind :: Integer -> [Integer] -> [Integer]
keepInMind = \x-> map (+x)

ff :: Int -> String
ff x = x+x

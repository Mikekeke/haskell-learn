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

-- more stuff to keep in mind: figuring out IO bind
-- wtf all that for look https://stepik.org/lesson/Монада-IO-8443/step/5?unit=1578

funRem1 x = (x+2, (show x))
funRem2 s x = (x*10, [show $ head s, (show x) ]) 

funIOExRemake :: (Int -> (Int, a)) ->  (a -> Int -> (Int, b)) -> (Int -> (Int, b))
funIOExRemake f k = \x -> case f x of (x', a) -> k a x'
testIOExRem = funIOExRemake funRem1 funRem2
-- figuring out IO bind - END

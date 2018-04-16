import           Debug.Trace

fact x = product [1..x]

f x
    | x == 25 = 2
    | x `mod` 5 == 0 = 1
    | otherwise = 0

zeros :: Int -> Int
zeros x = foldl (\b a -> b + (f a))  0  [1..x]

{-# LANGUAGE RankNTypes #-}

type Filter = (Int -> Bool) -> [Int] -> [Int]

filter1 :: Filter
filter1 _ []     = []
filter1 p (x:xs) = if p x then x : (filter1 p xs) else (filter1 p xs)

filter2 p = foldr (\x z -> if p x then x:z else z) []

filter3 p = foldr (\x -> if p x then (x:) else id) []

filters = [filter1, filter2, filter3]

tst :: [Filter] -> String
tst fs = let (h:t) = map (\f -> f (>3) [1,2,3,4]) fs
         in case all (h ==) t of
            True -> "Ok"
            _    -> "Not Ok"


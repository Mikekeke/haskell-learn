module Folds where
import Data.List


concatList :: [[a]] -> [a]
concatList = foldr (++) []

--my
meanList :: [Double] -> Double
meanList = (\(f,s) -> s/f) . foldr (\x (len, sum) -> (len+1, x+sum)) (0.0,0.0)

--better one not my
meanList' :: [Double] -> Double
meanList' = (uncurry (/)) . (foldr (\x (s, cnt) -> (s + x, cnt + 1)) (0, 0))

evenOnly :: [a] -> [a]
evenOnly = reverse . fst . foldl (\(l ,cnt) x  -> if even $ cnt then (x:l, cnt+1) else (l, cnt+1)) ([], 1)

--hmm, without ~ not gonna work on infinite lists
evenOnly' :: [a] -> [a]
evenOnly' = snd . foldr (\a ~(xs, ys) -> (a : ys, xs)) ([], [])


revRange :: (Char,Char) -> [Char]
revRange = unfoldr g
  where
    g (start, end)
        | end >= start = Just(end, (start, pred end))
        | otherwise = Nothing
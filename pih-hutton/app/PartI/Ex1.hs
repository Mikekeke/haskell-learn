module PartI.Ex1 where

product' []     = 1
product' (x:xs) = x * product' xs
test1 = product [2,3,4] == 24

qsort' [] = []
qsort' (x:xs) =
  qsort' (filter (>x) xs) ++ [x] ++ qsort' (filter (<=x) xs)
test2 = qsort' [3,4,1,1,5] == [5,4,3,1,1]


ex1Tests = all (==True) [test1, test2]

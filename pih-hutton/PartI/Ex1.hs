module PartI.Ex1 where

product' []     = 1
product' (x:xs) = x * product' xs
test1 = product [2,3,4] == 24

qsortRev [] = []
qsortRev (x:xs) =
  qsortRev (filter (>x) xs) ++ [x] ++ qsortRev (filter (<=x) xs)
test2 = qsortRev [3,4,1,1,5] == [5,4,3,1,1]

qsort'' [] = []
qsort'' (x:xs) =
  fh (>) ++ [x] ++ fh (<=) where fh f = qsort'' (filter (f x) xs)
test3 = qsort'' [3,4,1,1,5] == [1,1,3,4,5]


ex1Tests = [test1, test2, test3]

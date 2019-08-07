import Data.List

{-
https://www.codewars.com/kata/the-poet-and-the-pendulum/haskell
Just wrote it straight, thougt there will be some catches to optimize, but it passed tests. Whatever
-}
f ls = let (core:xs) = sort ls
           go [] _ l r = (l,r)
           go (a:as) flag l r = uncurry (go as (not flag)) $ if flag then (l, (a:r)) else ((a:l), r)
           (l',r') = go xs True [] []
       in l' ++ [core] ++ (reverse r')

{-
Cool from solution
====================
import qualified Data.Sequence as Seq
pendulum :: [Int] -> [Int]
pendulum xs = map (Seq.index sorted) indices where
  l = length xs - 1
  indices = reverse [0,2..l] ++ [1,3..l]
  sorted = Seq.fromList $ sort xs
====================
pendulum :: [Int] -> [Int]
pendulum xs = go [] [] (sort xs) where
  go left right (l:r:middle) = go (l:left) (r:right) middle
  go left right [x] = x : left ++ reverse right
  go left right [] = left ++ reverse right

-}
module FigureOut where
import           Data.List
import           Debug.Trace

perms :: (Eq a) => [a] -> [[a]]
perms []=    [[]]
perms (x:[])=[[x]]
perms l      = concatMap (\x -> map (\xs -> x:xs) (perms(filter (/=x) l))) l

--permutations
perms' :: [a] -> [[a]]
perms' [] = [[]]
perms' [x] = [[x]]
perms' (x:xs) = concatMap (insertElem x) (perms' xs) where
    insertElem x []         = [[x]]
    insertElem x yss@(y:ys) = (x:yss) : map (y:) (insertElem x ys)

groupElems3 :: Eq a => [a] -> [[a]]
groupElems3 [] = []
groupElems3 [x] = [[x]]
groupElems3 (x:xs)
 | x == head xs =
    let
        (r:rs) = groupElems3 xs
    in
        (x : r) : rs
 | otherwise = [x] : groupElems3 xs


--without '~' not gonna work on infinite lists
evenOnly' :: [a] -> [a]
evenOnly' = snd . foldr (\a ~(xs, ys) -> (a : ys, xs)) ([], [])



-- monad... don't know what
funTrX = (>>=) (+3) (\x y-> (trace (show x)) y == 20) -- при вызове funTrX 3, x = 23
funTrY = (>>=) (+3) (\x y-> (trace (show y)) y == 20) -- при вызове funTrY 3, y = 20
-- ⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄
tst = (>>=) (return (+4)) (return (==20)) 20 -- True
-- λ> :t (>>=)
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

-- λ> :t (return (+3))
-- (return (+3)) :: (Num a, Monad m) => m (a -> a)

-- λ> :t (>>=) (return (+3))
-- (>>=) (return (+3)) :: (Num a, Monad m) => ((a -> a) -> m b) -> m b

-- λ> :t (return (+3)
-- (return (+3)) 5 :: Num a => a -> a

-- + см. fogure_out_in_ghci.txt

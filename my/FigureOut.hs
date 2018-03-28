module FigureOut where
import           Control.Monad
import           Data.Char
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


-- some intuition?
-- Just (+3) <*> pure 5 == 8
-- Just (+) <*> pure 5 <*> pure 15 == 20
-- fmap (,) head :: [a] -> b -> (a, b)
-- fmap (,) head <*> Just :: [a] -> (a, Maybe [a])
-- (,,) <$> head <*> show <*> Just :: Show a => [a] -> (a, String, Maybe [a])

fnn1 :: a -> (a, Maybe a)
fnn1 = (,) <$> id <*> Just

fnn2 :: a -> (a, Maybe a)
fnn2 = liftM2 (,) id Just

-- fnn3 = liftM2 (,) head isUpper  --won't compile, need same monad


encode :: Eq a => [a] -> [(a, Int)]
encode xs = map (\xs' -> (head xs', length xs')) (group xs)

encode' :: Eq a => [a] -> [(a, Int)]
encode' = map (liftM2 (,) head length) . group
-- encode' = (liftM2 (,) head length).group

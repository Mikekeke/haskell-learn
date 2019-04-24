import Data.List
import Control.Applicative
import Debug.Trace

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
-- (.*.) `on` f = \x y -> f x .*. f y -- from Prelude
g `on` f = \x y -> f x `g` f y

splitBy :: Char -> String -> (String, String)
splitBy c = fmap tail . break (== c)
key :: String -> String
key = fst . splitBy ':'

tl = [
         ["1:11", "3:33", "0:00"]
        ,["2:22", "4:44"]
        ,["5:55", "1:111"]
     ]



onKeyCompare :: String -> String -> Bool
onKeyCompare = (<) `on` key

keyOrd = compare `on` key

newtype SST a = SST {unSST :: [a]} deriving Show -- shuld be hidden
toSST = SST . fmap (sortBy keyOrd) -- should be available cons
sliceMiddleSST (SST l) = (\(a,b) -> (SST a, SST b)) $ splitAt (length l `div` 2) l

(<++>) :: SST a -> SST a -> SST a
-- (<++>) a b = SST $ ((++) `on` unSST) a b 
{-
λ: :t (SST .)
(SST .) :: (a1 -> [a2]) -> a1 -> SST a2
λ: :t (on (++) unSST)
(on (++) unSST) :: SST a -> SST a -> [a]
-}
(<++>) = (SST .) . (on (++) unSST)

type Merge a = (a -> a -> Bool) -> [a] -> [a] -> [a]
merge :: (String -> String -> Bool) -> [String] -> [String] -> [String]
merge _ [] rs = rs
merge _ ls [] = ls
merge pp (l:ls) (r:rs)  = if pp l r 
                       then l : merge pp ls (r:rs)
                       else r : merge pp (l:ls) rs

-- todo deal with duplicate keys
compact :: SST [String] -> SST [String]
compact (SST []) = (SST [])
compact (SST [x]) = (SST [x])
compact (SST (a:b:[])) = SST [merge onKeyCompare a b]
compact sst = 
    compact (compact l <++> compact r) where
                (l,r) = sliceMiddleSST sst
                


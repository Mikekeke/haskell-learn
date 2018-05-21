import           Control.Applicative
import Data.List

tnth 0 ls = ls
tnth n ls = case drop n ls of
    []     -> []
    (x:xs) -> x : tnth n xs

tn' 0 ls = ls
tn' n ls = map snd . filter (\(i,v) -> i `mod` n == 0) . zip [0..] $ ls
    where p (i,_) = i `mod` n == 0

skips :: [a] -> [[a]]
skips s = go tnth 0 where
        l = length s
        go f x | x == l = []
               | otherwise = (f x s) : (go f (x+1))
-- not my
-- skips' n root@(_:xs) = [ x | (x,k) <- zip root [0..], mod k n == 0] ++ ...


localMaxima l = map (!!1) . filter (liftA2 (==) maximum (!!1) ) $ (win3 l) where
    win3 l' | length l' < 3 = []
            | otherwise = (take 3 l') : (win3 $ tail l')

localMaxima' lst@(x:y:z:xs) | x<y && z<y = [y] ++ next lst
                  | otherwise = [] ++ next lst
                  where next l = localMaxima' (tail l)
localMaxima' _ = []

-- not my
localMaxima'' lst@(x:y:z:_) = (if x<y && z<y then [y] else []) ++ localMaxima'' lst
localMaxima'' _ = []


tplate = [1..9]
tstList = [1,1,2,3,3,6,6,6]

proc [] _ = []
proc input tp = (map (\x -> if elem x input then '*' else ' ') tp) : (proc (fout tp input) tp)
 where
    fout [] l = l
    fout (x:xs) toFilter = fout xs (delete x toFilter)

main = mapM_ put










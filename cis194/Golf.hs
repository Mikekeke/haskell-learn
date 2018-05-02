import           Control.Applicative

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


localMaxima l = map (!!1) . filter (liftA2 (==) maximum (!!1) ) $ (win3 l) where
    win3 l' | length l' < 3 = []
            | otherwise = (take 3 l') : (win3 $ tail l')







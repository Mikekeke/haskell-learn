sliding :: Int -> [a] -> [[a]]

sliding 0 _ = []
sliding _ [] = []
sliding n ls | length ls <= n = [ls]
             | otherwise = go n ls where
                go n' (x:xs) | n' == 0 = [] :  sliding n (tail ls)
                               | otherwise = let (y:ys) = go (n' - 1) xs in (x : y) : ys

sliding' :: Int -> [a] -> [[a]]
sliding' n ls | n == 0 || null ls = []
              | length ls > n =  (take n ls)  : sliding' n (tail ls)
              | otherwise = [ls]

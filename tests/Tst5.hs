ls1 = [(+1), (*2)]
ls2 = [1,2,3]

comb :: [a -> a] -> [a] -> [a]
comb l1 l2 = [f x | f <- l1, x <- l2]

comb' :: [a -> a] -> [a] -> [a]
comb' l1 l2 = do
     f <- l1
     x <- l2
     [f x]

comb'' :: [a -> a] -> [a] -> [a]
comb'' fs xs = fs >>= flip map xs 
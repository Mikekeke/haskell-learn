nTimes n times
    | times == 0 = []
    | otherwise = n : nTimes n (times - 1)

second1 :: [a] -> a
second1 xs = head (tail xs)

second2 = head . tail
third = head . tail . tail

head' ((:)x xs) = x
tail' (x : xs) = xs
tail'' (_ : xs) = xs

second3 (_ : xs) = head xs
second4 (_ : x : _) = x
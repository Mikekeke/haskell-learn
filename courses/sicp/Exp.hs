square :: Integer -> Integer
square x = x * x
-- square = (*) <*> id

fastExp :: Integer -> Integer -> Integer
fastExp b n | n == 0 = 1
            | even n = square $ fastExp b (n `div` 2)
            | otherwise = b * fastExp b (n-1)

fastExpIter :: Integer -> Integer -> Integer
fastExpIter b 1 = b
fastExpIter b n = go 1 n where
    go :: Integer -> Integer -> Integer
    go a n' | n' == 1 = a
            -- odd bad
            | odd n' = if a == 1 then go b (n'-1)
                        else go (b * a) (n' - 1)
            -- even ok (?)
            | even n' = if a == 1 then go (square b) (div n' 2)
                        else go (square a) (div n' 2)

tst = sequenceA <$> sequenceA [fastExpIter,fastExp]
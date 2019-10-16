import Data.List
import Control.Monad

coins = [0.5,1,2,5]

res = filter ((10 ==) . sum) . nub . fmap sort . replicateM 8 $ coins
-- (0.28 secs, 123,346,416 bytes)

res2' = do
    c1 <- coins
    c2 <- coins
    guard (c1 <= c2)
    c3 <- coins
    guard (c2 <= c3)
    c4 <- coins
    guard (c3 <= c4)
    c5 <- coins
    guard (c4 <= c5)
    c6 <- coins
    guard (c5 <= c6)
    c7 <- coins
    guard (c6 <= c7)
    c8 <- coins
    guard (c7 <= c8)
    return [c1,c2,c3,c4,c5,c6,c7,c8]
    
res2 = filter ((10 ==) . sum) $ res2'
-- (0.01 secs, 848,576 bytes)

go ln sum' l = do
    if ln < 8 then do
        guard (sum' < 10)
        b <- coins
        guard (head l <= b)
        go (succ ln) (sum' + b) (b:l)
    else do
        guard (sum' == 10)
        return l

res3 = coins >>= \c1 -> go 1 c1 [c1]
-- (0.01 secs, 739,368 bytes) -- before "length l" was substituted by "ln" argument and "guard (sm < 10)" moved up
-- (0.01 secs, 609,520 bytes) -- current

allEq = all (res==) [res2, reverse <$> res3]

import Control.Applicative (ZipList(ZipList), getZipList)

f >$< x = getZipList . fmap f $ ZipList x
af >*< aa = getZipList $ ZipList af <*> ZipList aa

x1s = [1,2,3]
x2s = [4,5,6]
x3s = [7,8,9]
x4s = [10,11,12]

-- step 7
divideList :: Fractional a => [a] -> a
divideList []     = 1
divideList (x:xs) = (/) x (divideList xs)

divideList' :: (Show a, Fractional a) => [a] -> (String,a)
divideList' []     = ("1.0",1)
divideList' (x:xs) = (/) <$> (toLogEntry x, x) <*> divideList' xs where
    toLogEntry x' = "<-" ++ show x' ++ "/"
import Test.QuickCheck
import Control.Applicative
import Data.Function

--Q3.2
counter x = (\x -> x + 1) $ ((\x -> x + 1) x)

counterAns x = (\x -> x + 1)
                ((\x -> x + 1)
                    ((\x -> x) x))

tstQ32 = quickCheck $  (liftA2 (==) counter counterAns :: Int -> Bool)

-------
mCycle :: [a] -> [a]
mCycle xs = xs ++ mCycle xs

mCycle' :: [a] -> [a]
mCycle' xs = fix (xs ++)

-- hangs
-- tstCycle = quickCheck $ (liftA2 ((==) `on` (take 10)) mCycle mCycle' :: String -> Bool)

--Q8.
reverse1 [] = []
reverse1 (x:xs) = reverse1 xs ++ [x]

-- foldl
reverse2 l = go [] l where
    go acc [] = acc
    go acc (x:xs) = go (x:acc) xs

reverse3 l = go l id [] where
    go [] accF = accF
    go (x:xs) accF = (go xs accF) . (x:) 

reverse3_1 l = go l [] where
    go [] = id
    go (x:xs) = (go xs) . (x:) 

r3 l = foldr (\x b -> b . (x:)) id l [] 

tstRev myRev = quickCheck (liftA2 (==) reverse myRev :: [Int] -> Bool)
tstQ81 = mapM_ tstRev [reverse1, reverse2, reverse3, reverse3_1]

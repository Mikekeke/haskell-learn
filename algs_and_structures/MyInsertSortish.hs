import Control.Applicative

insert :: Ord a =>  a -> [a] -> [a]
insert a [] = [a]
insert a (x:xs) = if a <= x then a:x:xs else x : insert a xs 
insSort :: Ord a => [a] -> [a]
insSort [] = []
insSort (x:xs) = insert x (insSort xs)
{-
insSort [3,1,2]
~ insert 3 (insSort [1,2])
~ insert 3 (insert 1 (insSort [2]))
~ insert 3 (insert 1 (insert 2 (insSort []))))
~ insert 3 (insert 1 (insert 2 [])))
~ insert 3 (insert 1 ([2]))
~ insert 3 [1,2]
~ 1 : insert 3 [2]
~ 1 : 2 : insert 3 []
~ 1 : 2 : [3]
-}

data Test = Test {getA :: Int, getB :: String} deriving (Eq, Show)
testList = [Test 5 "ddd", Test 4 "bb", Test 1 "z", Test 2 "a", Test 3 "bb", Test 6 "eee"]

insWith _ x [] = [x]
insWith p x (y:[]) = case p x y of {True -> [y,x]; _ -> [x,y]}
insWith p x (y:ys) = case p x y of {True -> y : insWith p x ys; _ -> x:y:ys}

sortWith :: (a -> a -> Bool) -> [a] -> [a]
sortWith _ [] = [] 
sortWith _ [x] = [x] 
sortWith p (x:xs) = insWith p x (sortWith p xs) 

sortBy :: Ord b => (a -> b) -> [a] -> [a]
sortBy f = sortWith (\x1 x2 -> (f x1) >= (f x2))

predSrtWithGt = (\x1 x2 -> (length . getB $  x1) > (length . getB $ x2))
predSrtWithEq = (\x1 x2 -> (length . getB $  x1) == (length . getB $ x2))
predSrtWithNEq = (\x1 x2 -> (length . getB $  x1) /= (length . getB $ x2))
predSrtWithLt = (\x1 x2 -> (length . getB $  x1) < (length . getB $ x2)) 

tstSrtWith = (flip sortWith) testList
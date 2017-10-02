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

concat' :: [a] -> [a] -> [a]
-- can pattern match like this
[] `concat'` ys = ys
(x:xs) `concat'` ys = x : xs `concat'` ys

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x: xs)
    | odd x = x : oddsOnly xs
    | otherwise = oddsOnly xs

init' :: [a] -> [a]
init' []     = error "Empty list"
init' (x:[]) = []
 -- !!! or better init' [x] = []
 -- !!! or even better init' [_] = []
init' (x:xs) = x : init' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' [x] = x
sum' (x:xs) = x + sum' xs

maxIn :: (Ord a) => [a] -> a
maxIn [] = error "Empty list"
maxIn [x] = x
maxIn (x:xs) = x `max` maxIn xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' [x] = [x]
reverse' xs = (last xs) : reverse' (init xs)

reverse'' :: [a] -> [a]
reverse'' [] = []
reverse'' [x] = [x]
reverse'' l =
    let
        go [] acc = acc
        go (x:xs) acc = go  xs (x : acc)
    in
        go l []

isPalindromeMy :: Eq a => [a] -> Bool
isPalindromeMy [] = True
isPalindromeMy [x] = True
isPalindromeMy (x:xs) = (x == last xs) && isPalindromeMy (init xs)

-- missed this again lol
isPalindrome1 :: (Eq a) => [a] -> Bool
isPalindrome1 xs = xs == (reverse xs)

isPalindrome2 []  = True
isPalindrome2 [_] = True
isPalindrome2 xs  = (head xs) == (last xs) && (isPalindrome2 $ init $ tail xs)
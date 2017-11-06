import Data.Char

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

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs)(y:ys) = (x,y) : zip' xs ys

zip'' :: [a] -> [b] -> [(a,b)]
zip'' (x:xs)(y:ys) = (x,y) : zip' xs ys
zip'' _ _ = []

unzip' :: [(a,b)] -> ([a],[b])
unzip' [] = ([],[])
unzip' ((x,y):xys) =
  let (xs,ys) = unzip' xys
  in(x:xs, y:ys)

sum3my :: Num a => [a] -> [a] -> [a] -> [a]
sum3my [] [] [] = []
sum3my a b c =
  let
    take' [] = 0
    take' (x:xs) = x
    tail' (x:xs) = xs
    tail' _ = []
    hSum l1 l2 l3= (take' l1) + (take' l2) + (take' l3)
  in
    [hSum a b c] ++ sum3my (tail' a) (tail' b) (tail' c)

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 [] ys zs = sum3 [0] ys zs
sum3 xs [] zs = sum3 xs [0] zs
sum3 xs ys [] = sum3 xs ys [0]
sum3 (x:xs) (y:ys) (z:zs) = x+y+z : sum3 xs ys zs

groupElemsMy :: Eq a => [a] -> [[a]]
groupElemsMy [] = []
groupElemsMy l =
    let
        (xs, rest) = span(== head l) l
    in
        xs : groupElemsMy rest

-- groupElems2 :: Eq a => [a] -> [[a]]
-- groupElems2 [] = []
-- groupElems2 [x] = [[x]]
-- groupElems2 (x:xs)
--
--
--
-- go (x:y:t)
--     | x == y = x : go (y:t)
--     | otherwise = x : []


--FIGURE OUT!
groupElems3 :: Eq a => [a] -> [[a]]
groupElems3 [] = []
groupElems3 [x] = [[x]]
groupElems3 (x:xs)
 | x == head xs =
    let
        (r:rs) = groupElems3 xs
    in
        (x : r) : rs
 | otherwise = [x] : groupElems3 xs
--FIGURE OUT!

take' :: Int -> [a] -> [a]
take' n _ | n <= 0 = []
take' _ []         = []
take' n (x:xs)     = x : take (n-1) xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
  | p x = x : takeWhile' p xs
  | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p l@(x:xs)
  | p x = dropWhile' p xs
  | otherwise = l

readDigitsMy  :: String -> (String, String)
readDigitsMy [] = ("","")
readDigitsMy s = span isDigit s

readDigits  :: String -> (String, String)
readDigits = span isDigit

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj a b [] = []
filterDisj p1 p2 (x:xs)
  | (p1 x) || (p2 x) = x : filterDisj p1 p2 xs
  | otherwise = filterDisj p1 p2 xs

filterDisj2 :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj2 p1 p2 = filter (\x -> p1 x || p2 x)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort l@(x:xs) = (qsort $ filter (< x) xs) ++ (filter (==x) l) ++ (qsort $ filter (> x) xs)

--using dots composing example from stepik
qsortOther :: Ord a => [a] -> [a]
qsortOther [] = []
qsortOther xs@(x:xs') = ltx xs' ++ eqx xs ++ gtx xs' where
    ltx = qsortOther . filter (<x)
    gtx = qsortOther . filter (>x)
    eqx = filter (==x)

--dotted
concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f = concat . map f

--not dotted
concatMap'' :: (a -> [b]) -> [a] -> [b]
concatMap'' f xs = concat (map f xs)
--ex: concatMap (\x -> [x+10, x*10]) [1,2,3]

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x->[x^2, x ^3])

squares'n'cubesRec :: Num a => [a] -> [a]
squares'n'cubesRec [] = []
squares'n'cubesRec (x:xs) = x^2 : x^3 : squares'n'cubesRec xs

delAllUpper :: String -> String
delAllUpper = unwords . filter (any isLower) . words

-- perms :: [a] -> [[a]]
-- perms []= [[]]
-- perms l = concatMap (map )

-- swp :: [a] -> [[a]]
-- swp [] = [[]]
-- swp (x:[]) = [[x]]
-- swp (x1:x2:[]) = [[x2, x1]]
-- swp (x1:x2:xs) = [[x2, x1], [x1, swp (x2:xs)], [x2, swp(x1:xs)]]

max3My :: Ord a => [a] -> [a] -> [a] -> [a]
max3My = zipWith3 (\a b c -> maximum [a,b,c])

max3Other :: Ord a => [a] -> [a] -> [a] -> [a]
max3Other = zipWith3 ((max .) . max)


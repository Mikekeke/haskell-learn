-- https://www.hackerrank.com/challenges/largest-rectangle/problem

-- 1st idea: times out
getRect l (i, x) =  let
        getLen = length . takeWhile (>= x)
        (left, right) = splitAt i l
        l' =  getLen $ reverse left
        r' = getLen right
    in x * (l'+r')

largestRectangle1 = maximum . (map <$> getRect <*> zip [0..])

-- 2nd idea: passed tests
chop :: Eq a => a -> [a] -> [[a]]
chop _ [] = [[]]
chop s (x:xs) = if s == x 
    then [] : chop s xs 
    else let (xs':ys) = chop s xs in (x:xs') : ys

largestRectangle2 :: [Int] -> Int
largestRectangle2 [x] = x
largestRectangle2 l = let
        ln = length l
        lo = minimum l
        sq = (lo * ln) 
        chopped = filter (not . null) $ chop lo l
    in if null chopped then sq else max sq ( maximum $ largestRectangle2 <$> chopped)
    
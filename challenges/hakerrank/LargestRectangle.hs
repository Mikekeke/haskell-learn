-- https://www.hackerrank.com/challenges/largest-rectangle/problem
-- times out

getRect l (i, x) =  let
        getLen = length . takeWhile (>= x)
        (left, right) = splitAt i l
        l' =  getLen $ reverse left
        r' = getLen right
    in x * (l'+r')

gg = maximum . (map <$> getRect <*> zip [0..])
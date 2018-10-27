import           Data.List

zipSumTail :: [Int] -> [Int] -> [Int]
zipSumTail [] []             = []
zipSumTail [] r              = r
zipSumTail l []              = l
zipSumTail (lx:lxs) (rx:rxs) = lx + rx : (zipSumTail lxs rxs)

foldOnce :: [Int] -> [Int]
foldOnce [] = []
foldOnce [x] = [x]
-- will this save time?
-- foldOnce [x,y] = [x+y]
-- foldOnce [x,y,z] = [x+z,y]
foldOnce xs =
    let
        -- !!! turnPoint = (`div` 2) . length $ xs
        turnPoint = (length xs) `div` 2
        (l,r) = splitAt turnPoint xs
    in zipSumTail l (reverse r)


foldList :: [Int] -> Int -> [Int]
foldList l 0 = l
foldList l n = foldList (foldOnce l) (n-1)

-- iterate version from best solution
foldList' :: [Int] -> Int -> [Int]
foldList' = (!!) . iterate foldOnce

{-
bset sulution

foldLN :: [Int] -> [Int]
foldLN [] = []
foldLN [x] = [x] -- (my) difference will be only one element, so no need in {zipSumTail [] r = r; zipSumTail l [] = l}
foldLN (x:xs) = (x+(last xs)):(foldLN $ init xs)

foldList :: [Int] -> Int -> [Int]
foldList xs = (!!)(iterate foldLN xs)
-}

{-
intresting too: first list after "take" will be always <= than second
so it's safe to "((++ repeat 0) . take l $ xs" and then zip

foldList :: [Int] -> Int -> [Int]
foldList a 0 = a
foldList xs n = foldList (f xs) (pred n)
    where f xs = let l = (`div` 2) . length $ xs in zipWith (+) ((++ repeat 0) . take l $ xs) (reverse . drop l $ xs)
-}

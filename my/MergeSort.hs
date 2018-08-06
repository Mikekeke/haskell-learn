type Merge a = [a] -> [a] -> [a]
merge :: (Ord a, Integral a) => Merge a
merge [] xs = xs
merge xs [] = xs
merge l@(x:xs) r@(y:ys) = if x <= y then x: merge xs r else y:merge l ys


mSort :: (Ord a, Integral a) => Merge a -> [a] -> [a]
mSort _ [] = []
mSort _ [x] = [x]
mSort merg xs = 
  let 
    half = length xs `div` 2
    (l,r) = splitAt half xs
  in merg (mSort merg l) (mSort merg r) 
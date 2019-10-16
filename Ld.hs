import Data.List
import Control.Monad




changeL acc [] [] = acc
changeL acc w1 w2 | w1 == w2 = acc
changeL acc (o:os) (c:cs) | o == c = changeL acc os cs
                          | otherwise = changeL (succ acc) os cs
changeL _ _ _ = maxBound

removeL acc [] w2 = acc + length w2
removeL _ _ [] = maxBound 
removeL acc w1 w2 | w1 == w2 = acc
removeL acc w1@(o:os) (c:cs) | o == c = removeL acc os cs
                             | otherwise = removeL (succ acc) w1 cs

addL acc w1 [] = acc + length w1
addL acc [] w2 = maxBound
addL acc w1 w2 | w1 == w2 = acc
addL acc w1@(o:os) (c:cs) | o == c = addL acc os cs
                          | otherwise = addL (succ acc) w1 (o:c:cs)

calcLD f original word = (f 0 original word) 
    -- + abs (length original - length word)

change (o:os) (_:cs) = let w1 = (o:os); w2 = o:cs in (w1, w2)
add (o:os) w = let w1 = o:os; w2 = o:w in (w1, w2)
remove (o:os) (_:cs) = let w1 = o:os; w2 = cs in (w1, w2)


ld [] [] = 0
ld w1 w2 | w1 == w2 = 0
ld w1 w2 | null w1 || null w2 = abs (length w1 - length w2)
ld (o:os) (c:cs) | o == c = ld os cs
ld w1 w2 = succ . minimum . fmap process $ [change, add, remove] where
-- ld w1 w2 = 1 +  (minimum . fmap process $ [chL1, addL1, rmL1]) where
    process = (uncurry ld) . (\f -> f w1 w2)


fnd1 x = foldr (\x' b -> if x == x' then Just x else b ) Nothing


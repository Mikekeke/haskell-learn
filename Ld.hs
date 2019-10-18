import Data.List
import Control.Monad
import Data.Function
import qualified Data.Map.Strict as M
import Debug.Trace
import Data.Monoid




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

init :: M.Map  Int Int
init = M.empty

ld [] [] = 0
ld w1 w2 | w1 == w2 = 0
ld w1 w2 | null w1 || null w2 = abs (length w1 - length w2)
ld (o:os) (c:cs) | o == c = ld os cs
ld w1 w2 = succ . minimum . fmap process $ [change, add, remove] where
-- ld w1 w2 = 1 +  (minimum . fmap process $ [chL1, addL1, rmL1]) where
    process = (uncurry ld) . (\f -> f w1 w2)

-- findMostSimilar ws w  = snd . head . sortBy (compare `on` fst) . fmap calcLd $ ws where
--     calcLd cw = (ld w cw, cw)



type Predicate = String
ldm :: M.Map (String,String) Int -> Predicate -> Predicate -> String -> String -> Int
ldm mem p1 p2 [] [] = 0
ldm mem p1 p2 w1 w2 | w1 == w2 = 0
ldm mem p1 p2 w1 w2 | null w1 || null w2 = abs (length w1 - length w2)
ldm mem p1 p2 (o:os) (c:cs) | Just x <- M.lookup (o:p1,c:p2) mem =  trace "mem hit" x
ldm mem p1 p2 (o:os) (c:cs) | o == c = ldm (M.insert (o:p1,c:p2) 0 mem) (o:p1) (c:p2) os cs
ldm mem p1 p2 w1@(o:os) w2@(c:cs) = succ . minimum . fmap process $ [change, add, remove] where
    prev = maybe 0 id (M.lookup (p1,p2) mem)
    newMem = (M.insert (o:p1,c:p2) (1 + prev) mem)
    process = (uncurry (ldm (traceShowId newMem) (o:p1) (c:p2))) . (\f -> f w1 w2)

ldm' = ldm M.empty [] [] 

findMostSimilar2 ws w  = snd . head . sortBy (compare `on` fst) . fmap calcLd $ ws where
    calcLd cw = (ldm' w cw, cw)


fnd1 x = foldr (\x' b -> if x == x' then Just x else b ) Nothing
tst = M.empty & M.insert ("a", "b") 2 & M.insert ("b", "b") 3

--https://www.codewars.com/kata/pick-peaks/train/haskell

import Data.List
import Data.Function

data PickedPeaks = PickedPeaks { pos :: [Int], peaks :: [Int]} deriving (Eq, Show)
pickPeaks :: [Int] -> PickedPeaks
pickPeaks ls = 
    let l1 = zip [1..] ls
        
    in undefined

findFun ls = 
    let go :: [(Int,Int)] -> Ordering -> [(Int,Int)] -> [(Int,Int)]
        go acc _ [] = acc
        go acc _ [_] = acc
        go acc prevOr (x:xs) = case (ord, prevOr) of
            (GT, LT) -> go (x:acc) GT xs
            (EQ, _) -> go acc prevOr (x : tail xs)
            _ -> go acc ord xs
            where ord = (compare `on` snd) x (head xs)
    in  go [] EQ $ zip [0..] ls

toPeaks ls = foldl (\(PickedPeaks p ps) (pos1,x) -> PickedPeaks (pos1 : p) (x:ps)) (PickedPeaks [] []) ls

{-
best from solutions
pickPeaks :: [Int] -> PickedPeaks
pickPeaks ls = [b | (a,b,c) <- zip3 xs (tail xs) (drop 2 xs), a<b && b>c] & unzip & swap & uncurry PickedPeaks
    where xs = zip ls [0..] & groupBy (on (==) fst) & map head

info:    
- grpupBy to handle plato, takes then head by "map head" to get 1st in plato and only one possible 1st in not_plato
- & == swap $
new knowledge:
- u can compare touples, copares by 1st member; thats why "zip ls [0..]" and need swap after
- unzip :: [(a, b)] -> ([a], [b])

-}
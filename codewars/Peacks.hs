--https://www.codewars.com/kata/pick-peaks/train/haskell

import Data.List
data PickedPeaks = PickedPeaks { pos :: [Int], peaks :: [Int]} deriving (Eq, Show)
pickPeaks :: [Int] -> PickedPeaks
pickPeaks ls = 
    let l1 = zip [1..] ls
        
    in undefined

fun ls = 
    let l1 = zip [0..] ls
        go :: [(Int,Int)] -> Ordering -> [(Int,Int)] -> [(Int,Int)]
        go acc _ [] = acc
        go acc _ [_] = acc
        go acc prevOr (x:xs) | (snd x) > snd (head xs) && prevOr == LT = go (x:acc) GT xs
                             | (snd x) == snd (head xs) = go acc prevOr (x : tail xs)
                             | otherwise = go acc (compare (snd x) (snd $ head xs)) xs
    in reverse $ go [] EQ l1

toPs ls = foldr (\(pos1,x) (PickedPeaks p ps) -> PickedPeaks (pos1 : p) (x:ps)) (PickedPeaks [] []) ls
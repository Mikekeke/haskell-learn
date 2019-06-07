{-# LANGUAGE ConstrainedClassMethods #-}

import Control.Monad.State

class Boob a where 
    boob :: Show a => a -> String

instance Boob Int where boob = const "Int"
instance Boob Bool where boob = const "Bool"

-- https://www.hackerrank.com/challenges/common-child/problem?utm_campaign=challenge-recommendation&utm_medium=email&utm_source=60-day-campaign
l1 = "SHINCHAN"
l2 = "NOHARAAA"
stop = length l1

f :: Int -> [Int] -> Int -> String -> String -> [Int]
f it acs acc (x:xs) (y:ys) | acc > 0 && x /= y = f it acs (succ acc) xs (y:ys)
                           | x == y = f it acs (succ acc) xs ys
                           | x /= y = f it acs acc (x:xs) ys
                           | it == stop = acs


f it acs acc (_:xs) [] = f it (acc : acs) 0 xs l2
f it acs acc [] (_:ys) = let dr = succ it in 
    f dr (acc : acs) 0 (drop dr l1) ys





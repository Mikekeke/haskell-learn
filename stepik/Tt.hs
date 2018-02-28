module Tt where
import           Data.Function
import           Data.List

t1 :: (Integer, Integer) -> [Integer]
t1 = unfoldr (\(a,b) -> if a < b then Just(a+1,(a+1,b)) else Nothing)

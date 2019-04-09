module Tt where
import           Data.Function
import           Data.List

t1 :: (Integer, Integer) -> [Integer]
t1 = unfoldr (\(a,b) -> if a < b then Just(a+1,(a+1,b)) else Nothing)


tplls = [(1, 'a'), (2, 'b')]

fnd :: Integer -> Maybe Char
fnd = flip lookup tplls

newtype Arr k v = Arr {getArr:: k -> Maybe v}

tst = Arr fnd
extrList :: Arr a b -> [(a,b)]
extrList = undefined

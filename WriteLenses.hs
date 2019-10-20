import Test.QuickCheck
import Control.Applicative

-- https://vitez.me/building-lenses
newtype SimpleGetter s a = SimpleGetter (s -> a)


fst' :: SimpleGetter (a,b) a
fst' = SimpleGetter fst
tst1 = let (SimpleGetter f) = fst' in quickCheck (liftA2 (==) f fst :: (Int,Int) -> Bool)
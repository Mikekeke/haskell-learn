import System.IO
import Data.List
import Data.Char

l1 = [1,2,3]
l2 = [4,5]
f:: [Int] -> [Int] -> [Int]
fn = (:).head
ss :: [Int] -> [Int]

dd:::: (Applicative f) => f (a -> b) -> f a -> f b
dd = (<*>)

ss = fn <*> tail

res = fn l1 l2
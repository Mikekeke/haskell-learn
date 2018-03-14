import           Data.Char
import           Data.List
import           System.IO

l1 = [1,2,3]
l2 = [4,5]
fn:: [Int] -> [Int] -> [Int]
fn = (:).head

dd:: Applicative f => f (a -> b) -> f a -> f b
dd = (<*>)

ss :: [Int] -> [Int]
ss = fn <*> tail

res = fn l1 l2


func a b
    | Nothing <- a = -1
    | Nothing <- b = -2
    | Nothing <- a, Nothing <- b = -3
    | (Just x) <- a, (Just y) <- b = x+y

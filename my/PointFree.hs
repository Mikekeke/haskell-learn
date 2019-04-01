import Data.List
import Control.Applicative
import Control.Monad

ap' :: (a -> a -> a) -> (a -> a) -> (a -> a)
ap' f1 f2 = \x -> (f1 x) (f2 x)
{-
ap' (+) (+1) $ 3 ==> 7
same as 
(+) <*> (+1) $ 3 ==> 7
-}

if' p a b = case p of {True -> a; False -> b}


f1 x = if' (all null x) (take 1 x) x
f2 x = (if' (all null x)) <*> (take 1) $ x
f22 x = (if' (all null x)) <*> (take 1) $ x
-- completely pfree
f3 :: Foldable t =>  [t a] -> [t a]
f3 = join (ap (if' . all null) (take 1))

{-
λ: :t (+) . (*2)
(+) . (*2) :: Num a => a -> a -> a

λ: :t (+) <*> (*2)
(+) <*> (*2) :: Num a => a -> a


λ: :t (.).(.)
(.).(.) :: (b -> c) -> (a1 -> a -> b) -> a1 -> a -> c
λ: ff = (.).(.)
λ: ff (+1) (+) 10 20
31
λ: ff (*2) (+) 10 20
60
λ: (*2) . (+) $ 10 20

λ: :t join (+)
join (+) :: Num a => a -> a
λ: join (+) $ 3
6
-}
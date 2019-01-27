import Control.Monad (ap, return)
import Debug.Trace

-- decode c [x] = x
-- decode c (x:xs) = foldl (\b x -> if b < x then b*x else x+b) x xs

-- square x c = c (x^2)
-- add a b c = c (a+b)

decode c = c []
as [x] _ _ = x
as l _ _ = let (x:xs) = reverse l in foldl (\b y -> if b < y then b*y else y+b) x xs
a = undefined
number = undefined

cpsfy n b c = c (n:b)

one = cpsfy 1
two = cpsfy 2
three = cpsfy 3
seventeen = cpsfy 17
twenty = cpsfy 20
hundred = cpsfy 100
thousand = cpsfy 1000

-- ff = decode twenty as a number
test1 = decode one hundred twenty three as a number


newtype Cont r a = Cont {runCont :: (a -> r) -> r}

showCont :: Show a => Cont String a -> String
showCont m = runCont m show
-- from answers : showCont = (`runCont` show)

ret :: a -> Cont r a
ret x = Cont $ \c -> c x 


instance Functor (Cont r) where
    {-
    f :: a -> b
    g :: (a -> r) -> r
    c :: b -> r
    -}

    fmap f (Cont g) = Cont $ \c -> g (\a1 -> c (f a1))

instance Applicative (Cont r) where
    pure x = Cont $ \c -> c x
    {-
    apF :: ((a -> b) -> r) -> r
    f :: (a -> r) -> r
    c :: b -> r
    -}
    (Cont fa) <*> (Cont f) = 
        Cont $ \c -> f (\a1 -> fa(\g -> c (g a1)))

instance Monad (Cont r) where
    return x = Cont $ \c -> c x
--  (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
    (Cont f) >>= k = Cont $ \c -> f (\a1 -> runCont (k a1) c)


type Checkpointed a = (a -> Cont a a) -> Cont a a

-- was in work in progress
runCheckpointed' :: Show a => (a -> Bool) -> Checkpointed a -> a
-- runCheckpointed :: (a -> Bool) -> ((a -> Cont a a) -> Cont a a) -> a
runCheckpointed' p cp = runCont ff id where
    f = \y -> Cont $ \c -> if p (c y) then c y else y
    ff = cp f
-- was in work in progress - END

runCheckpointed :: (a -> Bool) -> Checkpointed a -> a
runCheckpointed p cp = runCont (cp f) id where
    f = \y -> Cont $ \c -> if p (c y) then c y else y
    
addTens :: Int -> Checkpointed Int
-- addTens :: Int -> ((Int -> Cont Int Int) -> Cont Int Int)
addTens x1 = \checkpoint -> do
  checkpoint x1
  let x2 = x1 + 10
  checkpoint x2     {- x2 = x1 + 10 -}
  let x3 = x2 + 10
  checkpoint x3     {- x3 = x1 + 20 -}
  let x4 = x3 + 10
  return x4

{-
GHCi> runCheckpointed (< 100) $ addTens 1
31
GHCi> runCheckpointed  (< 30) $ addTens 1
21
GHCi> runCheckpointed  (< 20) $ addTens 1
11
GHCi> runCheckpointed  (< 10) $ addTens 1
1
-}
        

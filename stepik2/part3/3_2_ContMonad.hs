import Control.Monad (ap, return)
import Debug.Trace
import Control.Monad.Except
import qualified Control.Monad.Cont as C

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

cont :: ((a -> r) -> r) -> Cont r a
cont = Cont


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
> runCheckpointed (< 100) $ addTens 1
31
> runCheckpointed  (< 30) $ addTens 1
21
> runCheckpointed  (< 20) $ addTens 1
11
> runCheckpointed  (< 10) $ addTens 1
1
-}

newtype FailCont r e a = FailCont {runFailCont :: (a -> r) -> (e -> r) -> r}
data ReadError = EmptyInput deriving Show


instance Functor (FailCont r e) where
    fmap f (FailCont g) = FailCont $ \ ok err -> g (\x -> ok (f x)) err
-- from solutions
-- instance Functor (FailCont r e) where fmap = liftM    

instance Applicative (FailCont r e) where
    pure = return
    (<*>) = ap

instance Monad (FailCont r e) where
    return x = FailCont $ \ok _ -> ok x
    (FailCont g) >>= k = FailCont $ \ok err -> g (\x -> runFailCont (k x) ok err) err
        

add :: Int -> Int -> FailCont r e Int
add x y = FailCont $ \ok _ -> ok $ x + y

toFailCont :: Except e a -> FailCont r e a
toFailCont ex = FailCont $ \ok err -> either err ok (runExcept ex)

evalFailCont :: FailCont (Either a b) a b -> Either a b
evalFailCont cnt = (runFailCont cnt) Right Left

tryRead :: String -> Except ReadError Int
tryRead "" = throwError EmptyInput
tryRead s = return . read $ s

addInts :: String -> String -> FailCont r ReadError Int
addInts s1 s2 = do
  i1 <- toFailCont $ tryRead s1
  i2 <- toFailCont $ tryRead s2
  return $ i1 + i2
        
callCFC :: ((a -> FailCont r e b) -> FailCont r e a) -> FailCont r e a
callCFC f = FailCont $ \ok err -> runFailCont (f $ \a1 -> FailCont $ \_ _ -> ok a1) ok err

callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = Cont $ \c -> runCont (f $ \a1 -> Cont $ \_ -> c a1) c

callCC' :: ((a -> (b -> r) -> r) -> (a -> r) -> r) -> (a -> r) -> r
callCC' f = \c -> f (\a1 _ -> c a1) c

testCC :: Num r => Integer -> Cont r Integer
testCC x1 = callCC $ \k -> do
    x2 <- return 10
    x3 <- cont (\c -> c (x2 + 100))
    when (x1 > 10) (k (-1))
    return $ x3
    
-- when      :: (Applicative f) => Bool -> f () -> f ()
-- when p s  = if p then s else pure ()
testCC2 :: Integer -> Cont r Integer
--                                                               k :: Integer -> Cont r ()
testCC2 x1 = callCC $ \k -> return 22 >>= \x2 -> when (x1 > 10) (k 101) >> return (x1 + x2)

-- ??????????????????
dd f = f (\a1 -> "lol " ++ show a1)
dd1 x = dd $ \fun -> if x > 10 then "ok" else fun x
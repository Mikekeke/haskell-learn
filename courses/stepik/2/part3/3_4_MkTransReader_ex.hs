import Data.Functor.Identity
import Control.Applicative

newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }

arr2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
-- arr2 f = Arr2T (fmap (fmap  return) f)
arr2 = Arr2T . (fmap  (fmap  return))
arr3 :: Monad m => (e1 -> e2 -> e3 -> a) -> Arr3T e1 e2 e3 m a
arr3 f = Arr3T (fmap (fmap  (fmap return))  f)

test1_1 = ((getArr2T $ arr2 (+)) 33 9 :: [Integer])                       == [42]
test1_2 = ((getArr3T $ arr3 foldr) (*) 1 [1..5] :: Either String Integer) == Right 120
test1_3 = (runIdentity $ (getArr2T $ arr2 (+)) 33 9)                      == 42
tests1 = [test1_1, test1_2,test1_3]

{-
arr2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
arr2 = Arr2T . ((.) . (.)) return
arr3 :: Monad m => (e1 -> e2 -> e3 -> a) -> Arr3T e1 e2 e3 m a
arr3 = Arr3T . ((.) . (.) . (.)) return
*********************************************
arr2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
arr2 = Arr2T . ((return.).)
arr3 :: Monad m => (e1 -> e2 -> e3 -> a) -> Arr3T e1 e2 e3 m a
arr3 = Arr3T . (((return.).).)
*********************************************
arr2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
arr2 f = Arr2T{getArr2T = (\e1 e2 -> return (f e1 e2))}
arr3 :: Monad m => (e1 -> e2 -> e3 -> a) -> Arr3T e1 e2 e3 m a
arr3 f = Arr3T{getArr3T = (\e1 e2 e3 -> return (f e1 e2 e3))}
-}

instance Functor m => Functor (Arr2T e1 e2 m) where
    fmap f arr2 = Arr2T $ (fmap . fmap . fmap $ f) (getArr2T arr2)
    -- fmap f = Arr2T . (fmap . fmap . fmap) f . getArr2T

instance Functor m => Functor (Arr3T e1 e2 e3 m) where
    fmap f arr3 = Arr3T $ (fmap . fmap . fmap . fmap $ f) (getArr3T arr3)
    -- fmap f = Arr3T . (fmap . fmap . fmap . fmap) f . getArr3T

a2l = Arr2T $ \e1 e2 -> [e1,e2,e1+e2]
test2_1 = ((getArr2T $ succ <$> a2l) 10 100) == [11,101,111]
a3e = Arr3T $ \e1 e2 e3 -> Right (e1+e2+e3)
test2_2 = (((getArr3T $ sqrt <$> a3e) 2 3 4):: Either String Double) == Right 3.0
tests2 = [test2_1, test2_2]

instance Applicative m => Applicative (Arr2T e1 e2 m) where
    pure x = Arr2T $ \_ _ -> pure x
    arr2L <*> arr2R = Arr2T $ \e1 e2 -> getArr2T arr2L e1 e2 <*> getArr2T arr2R e1 e2

instance Applicative m => Applicative (Arr3T e1 e2 e3 m) where
    pure x = Arr3T $ \_ _ _ -> pure x
    arr3L <*> arr3R = Arr3T $ \e1 e2 e3 -> getArr3T arr3L e1 e2 e3 <*> getArr3T arr3R e1 e2 e3

{- !!!
!to-remember
instance Applicative m => Applicative (Arr2T e1 e2 m) where
  pure = Arr2T . const . const . pure
  f <*> x = Arr2T $ (liftA2 . liftA2) (<*>) (getArr2T f) (getArr2T x)

instance Applicative m => Applicative (Arr3T e1 e2 e3 m) where
  pure = Arr3T . const . const . const . pure
  f <*> x = Arr3T $ (liftA2 . liftA2 . liftA2) (<*>) (getArr3T f) (getArr3T x)

*******************************************

instance Applicative m => Applicative (Arr2T e1 e2 m) where
  pure = Arr2T . pure . pure . pure
  (Arr2T f) <*> (Arr2T x) = Arr2T $ \a b -> f a b <*> x a b

instance Applicative m => Applicative (Arr3T e1 e2 e3 m) where
  pure = Arr3T . pure . pure . pure . pure
  (Arr3T f) <*> (Arr3T x) = Arr3T $ \a b c -> f a b c <*> x a b c
-}

instance Monad m => Monad (Arr2T e1 e2 m) where
    (Arr2T arr) >>= k = Arr2T $ \a b -> do
        v <-  arr a b
        getArr2T (k v) a b
--  (Arr2T x) >>= f = Arr2T $ \a b-> x a b >>= \y -> getArr2T (f y) a b

instance Monad m => Monad (Arr3T e1 e2 e3 m) where
    (Arr3T arr) >>= k = Arr3T $ \a b c -> do
        v <-  arr a b c
        getArr3T (k v) a b c
--  (Arr3T x) >>= f = Arr3T $ \a b c-> x a b c >>= \y -> getArr3T (f y) a b c

    fail s = Arr3T $ \_ _ _ -> fail s

{-
GHCi> a2l = Arr2T $ \e1 e2 -> [e1,e2]
GHCi> getArr2T (do {x <- a2l; y <- a2l; return (x + y)}) 3 5
[6,8,8,10]
GHCi> a3m = Arr3T $ \e1 e2 e3 -> Just (e1 + e2 + e3)
GHCi> getArr3T (do {x <- a3m; y <- a3m; return (x * y)}) 2 3 4
Just 81
-}

{-
fail in inner monad
getArr3T (do {10 <- a3m; y <- a3m; return y}) 2 3 4 == Nothing

not gona work locally, coz now "fail" in MonadFail
-}

class MonadTrans t where
    lift :: Monad m => m a -> t m a

instance MonadTrans (Arr2T e1 e2) where
    lift m = Arr2T $ \_ _ -> m

asks2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
asks2 f = Arr2T $ \a b -> return (f a b)

a2l_2 = Arr2T $ \e1 e2 -> [e1,e2]
testLift = (getArr2T (do {x <- a2l_2; y <- lift [10,20,30]; return (x+y)}) 3 4) == [13,23,33,14,24,34]


tstArr :: Arr2T t1 t Maybe (t1, t, (t1, t))
tstArr = do {x <- asks2 const; y <- asks2 (flip const); z <- asks2 (,); return (x,y,z)}
testAsks2 = getArr2T tstArr 'A' 'B' == Just ('A','B',('A','B')) 
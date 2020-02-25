import Control.Monad

newtype Reader r a = Reader {runReader :: r -> a}

instance Functor (Reader r) where
    fmap = liftM

instance Applicative (Reader r) where
    pure = return
    (<*>) = ap

instance Monad (Reader r) where
    return x = Reader $ \_ -> x
    (Reader f) >>= k = Reader $ \r -> runReader (k . f $ r) r

ask = Reader id

reader1 :: Reader [a] Int
reader1 = do
    x <- ask
    return (length x + 1)

{-
λ: runReader reader1 "asd"
4
-}

local :: (r -> r) -> Reader r a -> Reader r a
local g (Reader f) = Reader $ f . g

reader2 :: Reader [a] Int
reader2 = do
    x <- local tail ask
    return (length x + 1)

{-
λ: runReader reader2 "asd"
3
λ: runReader (local tail reader1) "asd"
3
-}

reader3 :: Reader String (String, Int)
reader3 = do
    x1 <- local (filter (> 'a')) ask >>= return . (++ "!!")
    x2 <- ask
    return (x1, length x2)
{-
λ: runReader reader3 "asd"
("sd!!",3)
-}

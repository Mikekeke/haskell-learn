import Control.Monad

f :: Integer -> Integer
f = join (*)
-- join (*) 4 => 16
-- join :: Monad m => m (m a) -> m a
-- join m = m >>= id
-- bimap :: Bifunctor p => (a -> b) -> (c -> d) -> p a c -> p b d
-- join bimap :: Bifunctor p => (c -> d) -> p c c -> p d d
-- (>>=) :: (t -> a) -> (a -> t -> b) -> (t -> b)
-- f >>= k = \e -> k (f e) e
-- easier analogy
-- join (*) 4 => 16
-- ~> (*) >>= id
{-
(>>=) :: (t -> a) -> (a -> t -> b) -> (t -> b)
(>>=) f k = \e -> k (f e) e
-}

t1 = (\a -> (\b -> (a*b))) >>= id
t2 = \t -> id ((\a -> (\b -> (a*b))) $ t) t
t3 = \t -> id (\b -> (t*b)) t
-- id (+1) 1
t4 = \t -> (\b -> (t*b)) t
t5 = \t -> (t*t)


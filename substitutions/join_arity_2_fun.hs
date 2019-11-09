import Control.Monad
{-
(>>=) :: (t -> a) -> (a -> t -> b) -> (t -> b)
(>>=) f k = \e -> k (f e) e
-}
t0 = join (*)
t1 = (\a -> (\b -> (a*b))) >>= id
t2 = \t -> id ((\a -> (\b -> (a*b))) $ t) t
t3 = \t -> id (\b -> (t*b)) t
-- id (+1) 1
t4 = \t -> (\b -> (t*b)) t
t5 = \t -> (t*t)
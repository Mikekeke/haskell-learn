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
-- ~> \x -> id ((*) x) x
-- ~> id ((*) 4) 4
-- ~> id (*4) 4
-- ~> id (4*4)
-- ~> id 16
-- ~> 16
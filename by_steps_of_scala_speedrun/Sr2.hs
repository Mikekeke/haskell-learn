{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

import           Data.Monoid
-- speedrun 2 video; not sure if I'm doing something alike their iso, lol


class Hzhz m where
    concWith :: Num a => (a -> m a) -> [a] -> a

instance Hzhz Sum where
    -- concWith :: (a -> Sum a) -> [a] -> a -- {-# LANGUAGE InstanceSigs #-} to enable uncommented
    concWith f xs = getSum $ mconcat $ map f xs

instance Hzhz Product where
    -- concWith :: (a -> Sum a) -> [a] -> a -- {-# LANGUAGE InstanceSigs #-} to enable uncommented
    concWith f xs = getProduct $ mconcat $ map f xs


testList = [2,3,4]

res :: (Num a, Hzhz m) => (a -> m a) -> [a] -> a
res mon ls = concWith mon ls

res1 = res Sum testList
res2 = concWith Product testList

class CIso a b where
    wrap :: a -> b
    unwrap :: b -> a

instance Num a => CIso a (Sum a)  where
    wrap = Sum
    unwrap = getSum

instance Num a => CIso a (Product a) where
    wrap = Product
    unwrap = getProduct


conc2 :: forall a b.(CIso a b, Monoid b) => a -> a -> b
conc2 a b = (wrap a) `mappend` (wrap b)
-- shit like this works
-- getProduct $ conc2 (2::Int) (4 :: Int) :: Int
-- getSum $ conc2 (2::Int) (4 :: Int) :: Int



main:: IO()
main =
    putStrLn ("Sum: " ++ (show res1)) >>
     putStrLn  ("Prod: " ++(show res2))

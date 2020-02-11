{-# LANGUAGE ScopedTypeVariables #-}

import Data.Foldable
import Control.Applicative

foldrM' :: forall a b t m . (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
foldrM' f z xs = foldr f' return xs z where
    f' :: a -> (b -> m b) -> b -> m b -- forall and pragma to enable this signature
    f' x k z' = f x z' >>= k

t1 = foldrM' (\a b -> Just (a:b)) [] [1,2]
t2 = foldr f' return [1,2] [] where f' x k z' = (\a b -> Just (a:b)) x z' >>= k
t3 = (foldr (\a k -> \b -> Just (a:b) >>= k) return [1,2]) []
t4 = (\b1 -> (Just (1:b1) >>= (foldr (\a k -> \b -> Just (a:b) >>= k) return [2]))) []
t5 = (\b1 -> 
        (Just (1:b1) >>= \b2 -> (Just (2:b2) >>= (foldr (\a k -> \b -> Just (a:b) >>= k) return [])))
     ) []
t6 = (\b1 -> (Just (1:b1) >>= \b2 -> (Just (2:b2) >>= return))) []     
t7 = (Just (1:[]) >>= \b2 -> (Just (2:b2) >>= return))
t8 = (Just (2:1:[]) >>= return)
t9 = Just (2:1:[])

tst = all (t1 ==) [t2,t3,t4,t5,t6,t7,t8,t9]

fish :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
-- fish k1 k2 a = k1 a >>= k2
fish k1 k2 = \a -> k1 a >>= k2

foldrM'' :: forall a b t m . (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
-- foldrM'' f z xs = foldr (\a b -> f a `fish` b) return xs z
foldrM'' f z xs = foldr (fish . f) return xs z

fn a b = Just (a+b)
tst2 = liftA2 (==) (foldrM' fn 0) (foldrM'' fn 0) [1,2,3]
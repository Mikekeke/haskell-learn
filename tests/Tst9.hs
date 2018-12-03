{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}


import Data.List
import Debug.Trace
import Control.Monad.State
import Control.Monad.Except
import Data.Monoid


foo, bar :: Int
foo = 10
bar = 100 

maker :: Monad m => m Int -> m Int
maker decider = do
    x1 <- return foo
    x2 <- decider
    x3 <- decider
    x4 <- return bar
    return (x1 + x2 + x3 + x4)

m1 = maker (Just 3) 
m2 = maker Nothing
m3 = maker (Right 3) 
m4 = maker (Left "kek")

-- collect monadic error?
-- data KekError b a = Good a | FuckUp b deriving Show

-- instance Monoid b => Functor (KekError b) where
--     fmap f (Good a) = Good $ f a
--     fmap _ (FuckUp b) = FuckUp b

-- instance Monoid b => Applicative (KekError b) where
--     pure  = return
--     (<*>) = ap

-- instance Monoid b => Monad (KekError b) where
--     return = Good
--     (Good a) >>= k = k a
--     (FuckUp b) >>= k = and what ???


grpByFld :: (a -> a -> Bool) -> [a] -> [[a]]
grpByFld p l@(x:y:xs) = foldr f [] l where
    f x1 [] = [[x1]] 
    f x1 (h:t)  | p x1 (head h) = (x1 : h) : t
               | otherwise = [x1] : h : t
grpByFld _ l = [l]

grpByReq :: (a -> a -> Bool) -> [a] -> [[a]]
grpByReq p (x1:x2:xs)
    | p x1 x2 = let (x2s:xxs) = groupBy p (x2:xs) in (x1:x2s):xxs
    | otherwise = [x1] : grpByReq p (x2:xs)
grpByReq _ l = [l] 


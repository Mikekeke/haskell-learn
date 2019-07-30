{-# LANGUAGE TupleSections #-}

import Data.List.NonEmpty as NE
import Control.Monad.Identity
import Prelude hiding (tail)
import Data.Function

l1 = 1 :| [2,3,4,5]
ix = flip (NE.!!)

takeS :: Int -> NonEmpty a -> [a]
takeS n = Prelude.take n . toList

-- https://www.youtube.com/watch?v=dOw7FRLVgY4&feature=youtu.be
takeTakeS n nel = takeS n nel' where 
    nel' :: NonEmpty [Integer]
    nel' = extend (takeS 3) nel
{-
λ: takeTakeS 6 infinite 
[[0,1,2],[1,2,3],[2,3,4],[3,4,5],[4,5,6],[5,6,7]]
-}

class Functor w => Comonad w where
    (=>>) :: w a -> (w a -> b) -> w b
    extend :: (w a -> b) -> w a -> w b
    counit :: w a -> a
    cojoin :: w a -> w (w a)
    x =>> f = fmap f (cojoin x)
    extend = flip (=>>)
    -- extend f = fmap f . cojoin
    -- cojoin wa = extend id wa
    cojoin = extend id

infinite :: NonEmpty Int
infinite = fromList [0..]

instance Comonad Identity where
    counit (Identity x) = x
    cojoin = Identity
    extend f wa = Identity (f wa)
    -- extend f = Identity . f
    -- extend = (Identity .)

instance Comonad NonEmpty where
    counit (a :| _) = a
    -- cojoin (a :| as) = (a :| as) :| go as where
    --     go [x] = [toNe [x]]
    --     go l = toNe l : go (Prelude.tail l)

    cojoin = unfold f where
        -- f (a :| as) = case as of
        --     [] -> ((a :| as), Nothing)
        --     (x:xs) -> ((a :| as), Just (x :| xs))
        f = (,) <*> (nonEmpty . tail)
        -- (,) :: t -> a -> (t, a)
        -- (nonEmpty . NE.tail) :: t -> a
        -- <*> :: (t -> a -> b) -> (t -> a) -> (t -> b)

(=>=) :: Comonad w => (w a -> b) -> (w b -> c) -> w a -> c
(=>=) k g wa = g $ fmap k (cojoin wa)
-- or
-- (=>=) k g = \wa -> g $ fmap k (cojoin wa)
-- or
-- (=>=) k g = g . fmap k . cojoin
-- or
-- (=>=) k g = g . extend k

composeTest = takeS 4 =>= ix 2
{-
λ: Prelude.take 4 $ composeTest infinite 
[2,3,4,5]
-}


winAvg :: Int -> NonEmpty Int -> Double
winAvg window = avg window . takeS window where 
    avg :: Int -> [Int] -> Double
    avg x l = ((/) `on` fromIntegral) (sum l) x


rollingAvg :: Int -> NonEmpty Int -> NonEmpty Double
rollingAvg window nel = extend (winAvg window) nel
-- rollingAvg = extend . winAvg

{-
takeS 5 $ (ix 2 <$> cojoin infinite) == takeS 5 $ (ix 2 $ cojoin infinite) == [2,3,4,5,6]
-}


data Store s a = Store (s -> a) s

instance Functor (Store s) where
    fmap f (Store g s) = Store (f . g) s

instance Comonad (Store s) where
    counit (Store f s) = f s
    -- cojoin (Store f s) = Store fn s where 
    --     -- fn :: s -> Store s a
    --     fn = Store f

    extend f sa@(Store g s) = Store (\s1 -> f sa) s

squared n = Store (\x -> x^2) n

tst :: Store s Int -> String
tst = show . counit . fmap (+1)
{-
λ: counit $ squared 2 =>> tst
"5"
λ: counit $ counit $ squared 2 =>> id
4
-}

experiment :: Functor f => (s -> f s) -> Store s a -> f a
experiment fn (Store g s) = fmap g $ fn s

{-
λ: experiment (\n -> [n-1,n+1]) (squared 2)
[1,9]
-}
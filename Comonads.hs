{-# LANGUAGE TupleSections #-}

import Data.List.NonEmpty as NE

l1 = 1 :| [2,3,4,5]

class Functor w => Comonad w where
    (=>>) :: w a -> (w a -> b) -> w b
    extend :: (w a -> b) -> w a -> w b
    counit :: w a -> a
    cojoin :: w a -> w (w a)
    x =>> f = fmap f (cojoin x)
    extend = flip (=>>)
    -- extend f = fmap f . cojoin

toNe (x:xs) = x :| xs

instance Comonad NonEmpty where
    counit (a :| _) = a
    -- cojoin (a :| as) = (a :| as) :| go as where
    --     go [x] = [toNe [x]]
    --     go l = toNe l : go (Prelude.tail l)

    cojoin = unfold f where
        -- f (a :| as) = case as of
        --     [] -> ((a :| as), Nothing)
        --     (x:xs) -> ((a :| as), Just (x :| xs))
        f = (,) <*> (nonEmpty . NE.tail)
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
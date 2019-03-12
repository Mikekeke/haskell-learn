{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

class Functor' c e | c -> e where
  fmap' :: (e -> e) -> c -> c

instance (Enum a, Functor f) => Functor' (f a) a where
    fmap' g s = fmap g s

-- GHCi> fmap' succ "ABC"
-- "BCD"
-- GHCi> fmap' (^2) (Just 42)
-- Just 1764

-- like, need to ensure, that functor's inside type same as function type? wtf is this exercise
{- from solutions
instance Functor' [a] a where fmap' = fmap
instance Functor' (Maybe a) a where fmap' = fmap
*****
instance Functor f => Functor' (f a) a where fmap' = fmap
-}
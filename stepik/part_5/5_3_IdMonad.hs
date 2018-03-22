newtype Identity a = Identity {runIdentity :: a} deriving (Eq, Show)

-- to make it work
instance Functor Identity where
    fmap  f (Identity x) = Identity (f x)

instance Applicative Identity where
    pure x = Identity x
    Identity f <*> Identity v = Identity (f v)

instance Monad Identity where
    return v = Identity v
    Identity v >>= k = k v
-- to make it work END

-- task 1 for Identity
-- instance Functor Identity where
--     fmap  f idt = idt >>= return .f
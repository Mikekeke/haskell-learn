import           Data.Char

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance Monad m => Functor (MaybeT m) where
    -- fmap f mt = MaybeT $ runMaybeT mt >>= \mb -> return $ fmap f mb
    fmap f mt = MaybeT $ runMaybeT mt >>= return . fmap f

instance Monad m => Applicative (MaybeT m) where
    pure = MaybeT . pure . Just
    mtf <*> mt = MaybeT $ do
        mf <- runMaybeT mtf
        mb <- runMaybeT mt
        pure $ mf <*> mb


instance Monad m => Monad (MaybeT m) where
    return = MaybeT . return . Just

    -- (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    m >>= k = MaybeT $ runMaybeT m >>= \mb -> case mb of
        Nothing -> return Nothing
        Just v  -> runMaybeT $ k v

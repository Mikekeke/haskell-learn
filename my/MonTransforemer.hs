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

liftToMt :: Monad m => a -> MaybeT m a
liftToMt = return

liftList :: a ->  MaybeT [] a
liftList = liftToMt

--test by getting value with "runMaybeT", wasn't able to do Show instance for it
t1 = (+3) `fmap` (liftList 4)
t2 = liftList (*10) <*> (liftList 4)
t3 = liftList 10 >>= \x -> return $ x - 400
t4help :: Maybe Int -> Maybe Int
t4help mb = mb >>= \x -> if x > 10 then Just (x*x) else Nothing
t4 x = liftList x >>= MaybeT .return . t4help

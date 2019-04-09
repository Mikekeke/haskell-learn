{-# Language InstanceSigs #-}

import Data.Functor.Identity
import Control.Applicative

newtype Reader r a = Reader {runReader :: r -> a}
newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

reader :: Monad m => (r -> a) -> ReaderT r m a
reader f = ReaderT (return . f)

instance Functor (Reader r) where
    fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap f r = Reader $ f . runReader r

instance Functor m => Functor (ReaderT r m) where
    fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
    fmap f rt = ReaderT $ fmap f . k where
        -- k :: r -> m a
        k = runReaderT rt

tst1 :: Maybe Int
tst1 = runReaderT (fmap succ $ reader (*3)) 7 -- Just 22
tst2 = runReaderT (fmap succ (ReaderT $ \e -> [1, e, e*2])) 7 -- [2,8,15]

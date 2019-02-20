import Control.Monad.Identity
import Control.Monad.Fail as F

data Logged a = Logged String a deriving (Eq,Show)

newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }

instance Monad m => Functor (LoggT m) where
    fmap = liftM

instance Monad m => Applicative (LoggT m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (LoggT m) where
    return = LoggT . pure . Logged ""
    m >>= k  = LoggT $ do 
                (Logged s a) <- runLoggT m
                (Logged s' a') <- runLoggT (k a)
                return $ Logged (mappend s s') a'

    -- fail msg = LoggT $ fail msg

instance MonadFail m => MonadFail (LoggT m) where
    fail msg = LoggT $ F.fail msg

logTst :: LoggT Identity Integer
logTst = do 
  x <- LoggT $ Identity $ Logged "AAA" 30
  y <- return 10
  z <- LoggT $ Identity $ Logged "BBB" 2
  return $ x + y + z
  
failTst :: [Integer] -> LoggT [] Integer
failTst xs = do
  5 <- LoggT $ fmap (Logged "") xs
  LoggT [Logged "A" ()]
  return 42

tests = [
    (runIdentity (runLoggT logTst)) == (Logged "AAABBB" 42)
    , (runLoggT $ failTst [5,5]) == [Logged "A" 42,Logged "A" 42]
    , (runLoggT $ failTst [5,6]) == [Logged "A" 42]
    , (runLoggT $ failTst [7,6]) == []
    ]
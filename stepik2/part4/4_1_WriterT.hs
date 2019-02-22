import Control.Monad.Identity
import Control.Monad.Trans.State
import Control.Monad.Fail as F
import Control.Monad.Trans.Class

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

instance MonadTrans LoggT where
    -- lift m = LoggT $ do
    --     v <- m
    --     return $ Logged "" v
    lift m = LoggT $ m >>= return . Logged ""
-- !!! lift = LoggT . fmap (Logged "")

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

tests1 = [
    (runIdentity (runLoggT logTst)) == (Logged "AAABBB" 42)
    , (runLoggT $ failTst [5,5]) == [Logged "A" 42,Logged "A" 42]
    , (runLoggT $ failTst [5,6]) == [Logged "A" 42]
    , (runLoggT $ failTst [7,6]) == []
    ]


write2log :: Monad m => String -> LoggT m ()
write2log s = LoggT . return $ Logged s ()
-- !!! write2log = LoggT . return . (`Logged` ())

type Logg = LoggT Identity

runLogg :: Logg a -> Logged a
runLogg = runIdentity . runLoggT

logTst' :: Logg Integer   
logTst' = do 
  write2log "AAA"
  write2log "BBB"
  return 42

stLog :: StateT Integer Logg Integer
stLog = do 
  modify (+1)
  a <- get
  lift $ write2log $ show $ a * 10
  put 42
  return $ a * 100

tests2 = [
    (runLogg logTst') == Logged "AAABBB" 42
    , (runLogg $ runStateT stLog 2) == Logged "30" (300,42)
    ]


logSt :: LoggT (State Integer) Integer
logSt = do 
  lift $ modify (+1)
  a <- lift get
  write2log $ show $ a * 10
  lift $ put 42
  return $ a * 100

tests3 = [(runState (runLoggT logSt) 2) == (Logged "30" 300,42)]
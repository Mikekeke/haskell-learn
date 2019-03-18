{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Functor.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Fail as F
import Control.Applicative

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


-- ****************************************************************8
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

instance MonadFail m => MonadFail (LoggT m) where
  fail msg = LoggT $ F.fail msg

instance MonadTrans LoggT where
  lift m = LoggT $ m >>= return . Logged ""

write2log :: Monad m => String -> LoggT m ()
write2log = LoggT . return . (`Logged` ())

type Logg = LoggT Identity

runLogg :: Logg a -> Logged a
runLogg = runIdentity . runLoggT

-- class MonadState' s m | m -> s where 
  
instance MonadState s m => MonadState s (LoggT m) where
  get   = lift get
  put   = lift . put
  state = lift . state -- !!! from answers: canuse just "state" as minimal required
  {-
    get :: m s
    get = state (\s -> (s, s))

    -- | Replace the state inside the monad.
    put :: s -> m ()
    put s = state (\_ -> ((), s))
  -}

logSt' :: LoggT (State Integer) Integer      
logSt' = do 
  modify (+1)                   -- no lift!
  a <- get                      -- no lift!
  write2log $ show $ a * 10
  put 42                        -- no lift!
  return $ a * 100


instance MonadReader r m => MonadReader r (LoggT m) where
  ask    = lift ask
  local fn lrm  = reader $ \r -> undefined 
  reader = lift . reader

mapLoggT :: (m (Logged a) -> n (Logged b)) -> LoggT m a -> LoggT n b
mapLoggT f = LoggT . f . runLoggT

logRdr :: LoggT (Reader [(Int,String)]) ()      
logRdr = do 
  m <- asks $ lookup 2                      -- no lift!
  write2log $ maybe "kek" id m
  -- Just y <- local ((3,"Jim"):) $ asks $ lookup 3 -- no lift!
  -- write2log y

lcl ::  (r -> r) -> Reader r a -> Reader r a
lcl f rm = reader $ \r -> runReader rm (f r)  
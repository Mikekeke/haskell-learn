import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Monad.Fail as F
import Data.Monoid (Sum)

newtype StateT s m a = StateT {runStateT :: s -> m (a,s)}

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT = fmap (fmap fst) . runStateT
 -- evalStateT =  (fmap fst .). runStateT
 -- evalStateT (StateT f) = (fmap fst) . f

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT = fmap (fmap snd) . runStateT

readerToStateT :: Monad m => ReaderT r m a -> StateT r m a
readerToStateT r = StateT $ \s -> runReaderT r s >>= return . flip (,) s
-- readerToStateT rT = StateT $ \r -> do
    --     x <- runReaderT rT r
    --     return (x,r)

-- !!! (from solutions) readerToStateT r = StateT $ \s -> fmap (\v -> (v,s)) $ runReaderT r s
-- !!! (from solutions) readerToStateT = StateT . (fmap <$> flip (,) <*>) . runReaderT


state :: Monad m => (s -> (a, s)) -> StateT s m a
state f = StateT (return . f)

-- execStateT :: Monad m => StateT s m a -> s -> m s
-- execStateT m = fmap snd . runStateT m 

-- evalStateT :: Monad m => StateT s m a -> s -> m a
-- evalStateT m = fmap fst . runStateT m

instance Functor m => Functor (StateT s m) where
  fmap f m = StateT $ \st -> fmap updater $ runStateT m st
    where updater ~(x, s) = (f x, s)

instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ \ s -> return (x, s)

  f <*> v = StateT $ \ s -> do
      ~(g, s') <- runStateT f s
      ~(x, s'') <- runStateT v s'
      return (g x, s'')

instance Monad m => Monad (StateT s m) where
  m >>= k  = StateT $ \s -> do
    ~(x, s') <- runStateT m s
    runStateT (k x) s'

instance MonadFail m => MonadFail (StateT s m) where
    -- fail msg = StateT $ \_ -> F.fail msg
    fail = StateT . const . F.fail

instance MonadTrans (StateT a) where
    lift m = StateT $ \s -> m >>= \a -> return (a,s)

get :: Monad m => StateT s m s
get = state $ \s -> (s, s)

put :: Monad m => s -> StateT s m ()
put s = state $ \_ -> ((), s)

modify :: Monad m => (s -> s) -> StateT s m ()
modify f = state $ \s -> ((), f s)


-- ******************************************************
data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving Show

numberAndCount :: Tree () -> (Tree Integer, Integer)
numberAndCount t = getSum <$> runWriter (evalStateT (go t) 1)
  where
    go :: Tree () -> StateT Integer (Writer (Sum Integer)) (Tree Integer)
    go t' = let succState = modify succ in case t' of 
        Leaf ()     ->  get >>= \i -> succState >> lift (tell $ Sum 1) >> return (Leaf i)
        Fork l () r -> 
            do
                l' <- go l
                i <- get
                succState
                r' <- go r
                return $ Fork l' i r'



-- !!! from solutions
pick :: StateT Integer (Writer (Sum Integer)) Integer
pick = state ((,) <$> id <*> (+1))
-- pick = state (\s -> (s, s+1))
go' :: Tree () -> StateT Integer (Writer (Sum Integer)) (Tree Integer)
go' (Leaf ())     = lift (tell (Sum 1)) >> Leaf <$> pick
go' (Fork l () r) = Fork <$> go' l <*> pick <*> go' r

{-
go' (Fork l () r) = p1 <*> pick <*> go' r
                where 
                    p1 :: StateT Integer (Writer (Sum Integer)) (Integer -> Tree Integer -> Tree Integer)
                    p1 = Fork <$> go' l

fnn :: Integer -> (Integer, Integer)
fnn = ((,) <$> id <*> (+1)) -- fnn 2 => (2,3)

just to campare and not forget:
λ: (+10) >>= (,) $ 1
(11,1)
-}
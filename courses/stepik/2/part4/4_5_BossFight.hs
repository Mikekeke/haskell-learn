{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- Пожалуйста, не удаляйте эти импорты. Они нужны для тестирующей системы.
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Cont
import Control.Monad.Identity
import Data.Foldable

f1 :: ContT Int Identity Int
f1 = undefined

newtype CoroutineT m a = CoroutineT { runCoroutineT :: ContT () m a} deriving (Functor, Applicative, Monad, MonadTrans, MonadCont)

idCC :: Monad m => a -> m ()
idCC = (\_ -> pure ())

runCoroutines :: Monad m => CoroutineT m () -> CoroutineT m () -> m ()
runCoroutines c1 c2 = runContT (runCoroutineT action) idCC 
    where
        action = do 
            c1
            c2

-- http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Cont.html#g:5
-- https://hackage.haskell.org/package/transformers-0.5.6.2/docs/Control-Monad-Trans-Cont.html#v:callCC
yield :: Monad m => CoroutineT m ()
yield = pure ()

testRun coro = runWriter $ (runContT . runCoroutineT $ coro) idCC
tst1  = testRun coroutine4
tst2  = testRun (coroutine4 >> coroutine3)

{-
λ: tst1
((),"abcd")
-}

instance MonadWriter w m => MonadWriter w (CoroutineT m) where
    tell = lift . tell
    listen = undefined
    pass = undefined

coroutine0 :: CoroutineT (Writer String) ()
coroutine0 = tell "@" >> yield >> tell "$"

coroutine1, coroutine2 :: CoroutineT (Writer String) ()
coroutine1 = do
    tell "1"
    yield
    tell "2"
  
coroutine2 = do
    tell "a"
    yield
    tell "b"

{-
> execWriter (runCoroutines coroutine1 coroutine2)
"1a2b"
-}


coroutine3, coroutine4 :: CoroutineT (Writer String) ()
coroutine3 = do
  tell "1"
  yield
  yield
  tell "2"

coroutine4 = do
  tell "a"
  yield
  tell "b"
  yield
  tell "c"
  yield
  tell "d"
  yield

{-
> execWriter (runCoroutines coroutine3 coroutine4)
"1ab2cd"
-}


coroutine_1, coroutine_2 :: CoroutineT (Writer String) ()
coroutine_1 = do
  tell "1"
  _ <- callCC $ \k -> do
    tell "cc1"
    k "Kek"
  tell "2"
  
coroutine_2 = tell "a" >> yield >> tell "b"

cntOneAndTwo = coroutine_1 >> coroutine_2

cntOneAndTwo' :: CoroutineT (Writer String) ()
cntOneAndTwo' = do 
  tell "1" 
  coroutine_2
  tell "2" 

-- runCtst :: Monad m => CoroutineT m () -> CoroutineT m () -> m ()
-- runCtst ca cb = 
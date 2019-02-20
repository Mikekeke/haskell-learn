{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# Language InstanceSigs #-}

import Control.Monad.Reader
import Control.Monad.State


tstR1 ::Reader [String] [Int]
tstR1 = do
    ss <- asks $ ("ddddd" :) 
    return $ map length ss
    
    
f = const ["ddddd"] 
tst1 = runReader tstR1 ["a", "bb", "ccc"]
tst2 = runReader (local f tstR1) ["a", "bb", "ccc"]

tstR2 ::Reader [String] [Int]
tstR2 = do
    ss <- local f ask
    return $ map length ss

tst3 = runReader tstR2 ["a", "bb", "ccc"]


{- ??????????????????????
from telegram
newtype App a = App
  { unApp :: ReaderT (Env App) IO a
  } deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (Env App)
    )
-}
newtype Env m = Env {getEnv :: [m String]}
newtype App a = App { unApp :: Reader (Env App)  a} deriving (Functor, Applicative, Monad, MonadReader (Env App)) 

instance Show (Env a) where
    show _ = "Env"

instance Show a => Show (App a) where
    show _ = "app"
    

runApp = runReader . unApp

initAp1 :: App String
initAp1 = return "test1"
initAp2 :: App String
initAp2 = return "test2"
testEnv :: Env App
testEnv = Env [initAp1, initAp2]

testAp :: App Int
testAp = do
--  aps :: App String
    aps <- asks (head . getEnv)
    x <- aps
    return $ read . drop 4 $ x

tst = runApp testAp testEnv

{-
λ: (runReader . unApp . asks last . getEnv) testEnv testEnv
"test2"
-}


fnn :: Reader Int [Int]
fnn = reader (\x -> [x, x*2, x^2])
fnn' = do
 -- x :: [Int]
    x <- fnn
    return undefined

res :: Reader Int [Int]
res = reader (\x -> [x+2, x*2, x^2])
res2 = do
    x1 <- res
    x2 <- ask
    x3 <- local (*33) ask
    return $ x3:x2:x1

testRes x = runReader res2 x
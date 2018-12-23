{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity

type Config = [(String, String)]

config = [("path", "test/path"), ("file", "filename")]

newtype App a = App {runApp :: ReaderT Config (Either String) a} deriving (Functor, Applicative, Monad, MonadReader Config)
test1 :: App String
test1 =  show <$> asks (lookup "path") 

r :: Reader [a] a
r = reader head

rt :: Monad m =>  ReaderT [a] m a
rt = reader head


{-************************************************************************
 import Control.Monad.Reader
 import Control.Monad.State
 import Debug.Trace

 class GetAlg m where
     getX :: Int -> m a


 mb = Just "new"

 type App = ReaderT String (StateT String Maybe)

 tst1 :: App String
 tst1 = do
     x <- get
     y <- ask
     n <- lift . lift $ mb
     modify ((n ++ " ") ++)
     return $ x ++ y
-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

import Data.Char
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Except

type Id = Int
data User = User {name :: String} deriving Show
data AppError = NotFound | NotValid | UserNotFound | BadConnection deriving Show

-- http://oleg.fi/gists/posts/2017-03-03-servant-and-db.html

data Connection = Dummy | Good

getUser :: Int -> Connection -> Either AppError User
getUser 1 c = Right $ User "test1"
getUser _ c = Left UserNotFound

validateUser u = case isUpper . head . name $ u of 
            True -> Right u
            _ -> Left "Invalid name"

class (Monad m) => DatabaseAlg m where
    getEntry :: (Connection -> Either AppError a) -> m a

class Monad m => ValidationAlg m where
    checkEntry :: (a -> m a) -> a -> m a

test1 :: (DatabaseAlg m, ValidationAlg m) => (Connection -> Either AppError a) -> m a
test1 c = do 
    ent <- getEntry c
    return ent


newtype App m a = App {getApp:: ReaderT Connection (ExceptT AppError m) a} 
    deriving (Functor, Applicative, Monad, MonadReader Connection, MonadError AppError)

runApp app db = runExceptT (runReaderT (getApp app) db)

instance DatabaseAlg (App IO) where
    getEntry f = do
        conn <- ask
        case conn of
            Good -> asks f >>= either throwError return
            Dummy -> throwError BadConnection

instance ValidationAlg (App IO) where
    checkEntry f e =  undefined

instance DatabaseAlg (App Identity) where
    getEntry f = ask >>= \case
            Good -> asks f >>= either throwError return
            Dummy -> throwError BadConnection

    -- getEntry f = do
        -- conn <- ask
        -- case conn of
            -- Good -> asks f >>= either throwError return
            -- Dummy -> throwError BadConnection

instance ValidationAlg (App Identity) where
    checkEntry f e =  undefined

t1 :: Identity (Either AppError User)
t1 = runApp (test1 (getUser 1)) Good
t2 :: Identity (Either AppError User)
t2 = runApp (test1 (getUser 4)) Good
t3 :: Identity (Either AppError User)
t3 = runApp (test1 (getUser 1)) Dummy


main :: IO ()
main = mapM_ (putStrLn . show . runIdentity) [t1,t2,t3]

-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Char
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Except

type Id = Int
data User = User {name :: String} deriving Show
type Database a = [(Int, a)]
data AppError = UserNotFound | UserNotValid deriving Show

-- http://oleg.fi/gists/posts/2017-03-03-servant-and-db.html

data Connection = Empty

class Monad m => DatabaseAlg m where
    getEntry :: (Connection -> Id -> m a) -> m a

class Monad m => ValidationAlg m where
    checkEntry :: a -> m (Either AppError a)

test1 :: (DatabaseAlg m, ValidationAlg m) => m a
test1 = do ent <-  undefined
           return ent



-- try make work ReaderT at least
newtype App m a = App {getApp:: ReaderT Connection (ExceptT AppError m) a} deriving (Functor, Applicative, Monad, MonadError AppError)

t1 :: App IO User
t1 = undefined
ts :: ReaderT [a] Identity a
ts = do e <- asks head
        return e

-- testState1 = []
-- runApp app = runExceptT (runStateT (getApp app) testState1)


-- instance DatabaseAlg (AppUser IO) where
--     getUser id_ = do user <- gets (lookup id_)
--                      case user of
--                         Just u -> return u
--                         _ -> throwError UserNotFound

--     insertUser user = do db <- get
--                          case null db of
--                             True -> do put [(1, user)]
--                                        return 1
--                             False -> do let nextId = succ . fst . last $ db
--                                         modify ((nextId, user) : )
--                                         return nextId

-- instance DatabaseAlg (AppUser Maybe) where
--     getUser id_ = do 
--                     user <- gets (lookup id_)
--                     case user of
--                         Just u -> return u
--                         _ -> throwError UserNotFound

--     insertUser user = do db <- get
--                          case null db of
--                             True -> do put [(1, user)]
--                                        return 1
--                             False -> do let nextId = succ . fst . last $ db
--                                         modify ((nextId, user) : )
--                                         return nextId

-- instance ValidationAlg (AppUser Maybe) where
--     checkUser u = case isUpper . head . name $ u of 
--         True -> return (Right u)
--         _ -> throwError UserNotValid
-- -- main1 :: IO (Either AppError (User, Database User))
-- -- main1 = runApp test1

-- main2 :: Maybe (Either AppError (User, Database User))
-- main2 = runApp test1



-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Char
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Except

type Id = Int
data User = User {name :: String} deriving Show
type Database a = [(Int, a)]
data AppError = UserNotFound | UserNotValid deriving Show

class Monad m => DatabaseAlg m where
    getUser :: Id -> m User
    insertUser :: User -> m Id

class Monad m => ValidationAlg m where
    checkUser :: User -> m (Either AppError User)

test1 :: (DatabaseAlg m, ValidationAlg m) => m User
test1 = do id_ <- insertUser (User "TestName")
           let u1 = (User "testName2")
           checkUser u1
           id_2 <- insertUser u1
           getUser 2



newtype AppUser m a = AppUser {getApp:: StateT (Database User) (ExceptT AppError m) a} deriving (Functor, Applicative, Monad, MonadState (Database User), MonadError AppError)

testState1 = []
runApp app = runExceptT (runStateT (getApp app) testState1)


instance DatabaseAlg (AppUser IO) where
    getUser id_ = do user <- gets (lookup id_)
                     case user of
                        Just u -> return u
                        _ -> throwError UserNotFound

    insertUser user = do db <- get
                         case null db of
                            True -> do put [(1, user)]
                                       return 1
                            False -> do let nextId = succ . fst . last $ db
                                        modify ((nextId, user) : )
                                        return nextId

instance DatabaseAlg (AppUser Maybe) where
    getUser id_ = do 
                    user <- gets (lookup id_)
                    case user of
                        Just u -> return u
                        _ -> throwError UserNotFound

    insertUser user = do db <- get
                         case null db of
                            True -> do put [(1, user)]
                                       return 1
                            False -> do let nextId = succ . fst . last $ db
                                        modify ((nextId, user) : )
                                        return nextId

instance ValidationAlg (AppUser Maybe) where
    checkUser u = case isUpper . head . name $ u of 
        True -> return (Right u)
        _ -> throwError UserNotValid
-- main1 :: IO (Either AppError (User, Database User))
-- main1 = runApp test1

main2 :: Maybe (Either AppError (User, Database User))
main2 = runApp test1



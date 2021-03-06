-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Char
import Control.Monad.State
import Control.Monad.Identity
import           Control.Monad.Except

type Id = Int
data User = User {name :: String} deriving Show

class Monad m => DatabaseAlg m where
    getUser :: Id -> m User
    insertUser :: User -> m Id

test1 :: DatabaseAlg m => m User
test1 = do id_ <- insertUser (User "TestName")
           id_2 <- insertUser (User "TestName2")
           getUser 10

type Database a = [(Int, a)]
data AppError = UserNotFound deriving Show

newtype AppUser m a = AppUser {unApp:: StateT (Database User) m a} deriving (Functor, Applicative, Monad, MonadState (Database User))
-- newtype AppUser2 a = AppUser2 {unApp2:: State [(Int, User)] a} deriving (Functor, Applicative, Monad)

testState1 = []
runApp app = runStateT (unApp app) testState1


instance DatabaseAlg (AppUser IO) where
    getUser id_ = do user <- gets (lookup id_)
                     case user of
                        Just u -> return u
                        _ -> fail "my-err"

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
                        _ -> fail ""

    insertUser user = do db <- get
                         case null db of
                            True -> do put [(1, user)]
                                       return 1
                            False -> do let nextId = succ . fst . last $ db
                                        modify ((nextId, user) : )
                                        return nextId
main1 :: IO (User, Database User)
main1 = runApp test1

main2 :: Maybe (User, Database User)
main2 = runApp test1



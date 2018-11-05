{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Char

data User = User {name :: String} deriving Show

class DbAlg m where
    vaidateNewName :: String -> m String
    dbFind :: [(Int,User)] -> Int -> m User
    renameEntry :: User -> String -> m User

database :: [(Int, User)]
database = map (fmap User) [(1,"Bob"), (2,"Tom")]

process :: (Monad m, DbAlg m) => [(Int,User)] -> Int -> String -> m User
process db id_ name_ = do
    u <- dbFind db id_
    res <- renameEntry u name_
    return res

instance DbAlg Maybe where
    vaidateNewName name_ = 
        case any (not . isAlpha) name_ of {True -> Nothing; _ -> Just name_}
    dbFind db id_ = lookup id_ db
    renameEntry (User n)  name_= vaidateNewName name_ >>= return . User

instance DbAlg (Either String) where
    vaidateNewName name_ = 
        case any (not . isAlpha) name_ of {True -> Left ("Illegan new name: " ++ name_); _ -> Right name_}
    dbFind db id_ = maybe (Left "User not found") Right (lookup id_ db)
    renameEntry (User n)  name_= vaidateNewName name_ >>= return . User

instance DbAlg (Either Int) where
    vaidateNewName name_ = 
        case any (not . isAlpha) name_ of {True -> Left 0; _ -> Right name_}
    dbFind db id_ = maybe (Left 0) Right (lookup id_ db)
    renameEntry (User n)  name_= vaidateNewName name_ >>= return . User
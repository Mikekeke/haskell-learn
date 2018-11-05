{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

data User = User {name :: String} deriving Show

class DbAlg m where
    dbFind :: [(Int,User)] -> Int -> m User
    renameEntry :: User -> String -> m User

protectedNames = ["Tom"]

database :: [(Int, User)]
database = map (fmap User) [(1,"Bob"), (2,"Tom")]

process :: (Monad m, DbAlg m) => [(Int,User)] -> Int -> String -> m User
process db id_ name_ = do
    u <- dbFind db id_
    res <- renameEntry u name_
    return res

instance DbAlg Maybe where
    dbFind db id_ = lookup id_ db
    renameEntry (User n)  name_= case elem n protectedNames of 
        True -> Nothing
        _ -> Just (User name_)


instance DbAlg (Either String) where
    dbFind db id_ = maybe (Left "User not found") Right (lookup id_ db)
    renameEntry (User n)  name_= case elem n protectedNames of 
        True -> Left "Protected user"
        _ -> Right (User name_)

instance DbAlg (Either Int) where
    dbFind db id_ = maybe (Left 0) Right (lookup id_ db)
    renameEntry (User n)  name_= case elem n protectedNames of 
        True -> Left 0
        _ -> Right (User name_)
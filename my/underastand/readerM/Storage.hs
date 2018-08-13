module Storage where

import Control.Monad.Reader
type Database = [(Int, String)]
storage :: Database
storage = [(1, "Bob"), (2,"Tom")]

getById :: Int -> ReaderT Database Maybe String
getById _id = do
    res <- asks $ lookup _id
    return $ maybe "Noman" id res

-- runReaderT (getById 1) storage ~> Just "Bob"
-- runReaderT (getById 3) storage ~> Just "Noman"

getById2 :: Int -> Reader Database (Maybe String)
getById2 _id = asks $ lookup _id

getById3 :: Int -> Reader Database (Maybe String)
getById3 _id = reader (lookup _id)
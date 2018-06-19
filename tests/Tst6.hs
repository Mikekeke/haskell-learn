-- did that while reading https://medium.com/@agaro1121/free-monad-vs-tagless-final-623f92313eac
-- need to revisit


{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

type ID = Int

data User = User {name :: String} | NoUser deriving Show
data Storage a = Storage {items :: ID -> a }

-- like that? possible good?
-- class Storage a where
--     storage :: ID -> a

-- instance Storage User where
--     storage 1 = User "Bob"
--     storage 2 = User "Tom"
--     storage 3 = NoUser

type Interpreter m a = Storage a -> ID -> m a
class (Monad m) => StorageAlg m a where
    getByIdAlg :: Interpreter m a

-- kind a variant, but together with below will result in overlapping instances
-- needs FlexibleInstances
-- instance Monad m => StorageAlg m User where
--     getByIdAlg st id = case (items st) id of
--                 u@(User _) -> return u
--                 NoUser     -> fail "error"
--                 _          -> error "kek while searching"

instance StorageAlg Maybe User where
    getByIdAlg st userId = case (items st) userId of
        u@(User _) -> Just u
        NoUser     -> Nothing
        _          -> error "kek while searching"

instance StorageAlg (Either String) User where
    getByIdAlg st userId = case (items st) userId of
        u@(User _) -> Right u
        NoUser     -> Left "user not found"
        _          -> error "kek while searching"

userFromStorage :: (Monad m) => Storage User -> ID -> Interpreter m User -> m User
userFromStorage st userId stAlg = stAlg st id

testUserStorage :: Storage User
testUserStorage = Storage $ \userId -> case userId of
    1 -> User "Bob"
    2 -> User "Tom"
    _ -> NoUser

-- auto-interpretation depending on type
testRun1 :: ID -> Maybe User
testRun1 userId = userFromStorage testUserStorage userId getByIdAlg
testRun2 :: ID -> Either String User
testRun2 userId = userFromStorage testUserStorage userId getByIdAlg

anotherEthInterpret :: Interpreter (Either String) User
anotherEthInterpret st userId = case (items st) userId of
                                User name_ -> Right . User $ name_ ++ " " ++ "by another"
                                NoUser     -> Left "user not found by another"

-- interps depending on type of anotherEthInterpret
testRun3 :: ID -> Either String User
testRun3 userId = userFromStorage testUserStorage userId anotherEthInterpret

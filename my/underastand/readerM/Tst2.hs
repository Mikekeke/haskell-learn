import           Control.Monad.Reader


data Thing a = Thing {getThing :: a -> [a]}
testTh = Thing $ replicate 4

rdr :: a -> Reader (Thing a) [a]
rdr x = do
    th <- ask
    return $ getThing th x

rdr' :: a -> Reader (Thing a) [a]
rdr' x = ask >>= \th -> return $ getThing th x

rdr'' :: a -> Reader (Thing a) [a]
-- rdr'' x = asks . flip getThing $ x
rdr'' = asks . flip getThing



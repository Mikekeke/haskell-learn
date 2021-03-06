import Control.Monad.Reader
import Control.Monad.Writer

t :: Writer [Int] String
t = do
    tell [3]
    tell [3]
    return "ok"

test1 :: ReaderT [String] (Writer String) String
test1 = do
    e1 <- asks (!!1)
    e2 <- asks (!!2)
    tell $ "out" ++ e2
    tell $ "out" ++ e1
    return e1

test2 :: ReaderT [String] (Writer String) String
test2 = do
    e1 <- asks (!!1)
    e2 <- asks (!!2)
    lift $ tell $ "out" ++ e2
    tell $ "out" ++ e1
    return e1

ss = ["1", "2", "3"]
tstTrs1 = (==) <$> runReaderT test1 <*> runReaderT test2 -- for ss is True

tst3 :: Monad m => ReaderT String (WriterT String m) String
tst3 = do
    x <- ask
    tell $ "one " ++ show x
    tell $ "two " ++ show x
    return x

{-
λ: runWriterT $ runReaderT ((show . length) <$> tst3) "test" :: Maybe (String, String)
Just ("4","one \"test\"two \"test\"")
-}
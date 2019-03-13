
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-} -- for types in lambdas

import System.IO
import Data.IORef
import Control.Monad.Writer hiding (fix)
import Control.Monad.Except hiding (fix)
import Control.Monad.State hiding (fix)
import Control.Monad.Reader hiding (fix)
import Control.Exception as Ex

type Entry = ([Char], ([Char], [Integer]))
-- ex :: [Entry]
ex = [("a", [("a11", [1,2]), ("a12", [3,4])])]

f :: Monoid b => [(a,b)] -> b
f = mconcat . map snd

-- append :: Entry -> [Entry] -> [Entry]
-- append en ens = 


fix f = let x = f x in x
fn = \rec' x -> if (x < 0) then rec' (x + 3) else x
test1 = fix fn (-10)



incRef :: IORef Int -> IO ()
incRef var = do
    val <- readIORef var
    writeIORef var (val+1)

type App1 = ExceptT String (StateT String IO) ()

run1 = runStateT (runExceptT (action `catchError` hErr)) "|" 
    where 
        action :: App1
        action = do
            modify ('a':)
            error "err"
            modify ('b':)
        hErr :: String -> App1
        hErr e = do
            modify ('E':)

data TestException = TestException deriving Show
instance Exception TestException

brokenRead = readFile "not_existed.txt"

type AppLoseState = StateT [String] IO ()

errSt :: AppLoseState
errSt = do
        modify ("err st: before read" : )
        s <- liftIO brokenRead
        modify (s:)
        modify ("err st: after read" : )

okSt :: AppLoseState
okSt = modify ("ok state: ok" : )

        

runErrMain :: AppLoseState     
runErrMain = do
    s <- get
    (r1, s1) <- liftIO $ (runStateT errSt s)
        -- no way to get state from failed "errSt"
        `catch` (\(e :: IOException) -> return ((), s))
    put s1
    (r2, s2) <- liftIO $ (runStateT okSt s1)
        `catch` (\(e :: IOException) -> return ((), s1))
    put s2
    modify ("END" :)

tst1 = runStateT runErrMain [] -- ((),["END","ok state: ok"])

type AppNotLoseState = ReaderT (IORef [String]) IO ()

errRd :: AppNotLoseState
errRd = do
    var <- ask
    liftIO $  modifyIORef var ("err rd: before read" :)
    liftIO brokenRead
    liftIO $  modifyIORef var ("err rd: after read" : )

okRd :: AppNotLoseState
-- okRd = ask >>= \var -> liftIO $ modifyIORef var ("ok rd: ok" : )
okRd = ask >>= liftIO . flip modifyIORef ("ok rd: ok" :)

runRdMain :: AppNotLoseState     
runRdMain = do
    -- st <- liftIO $ 
    st <- ask
    x1 <- liftIO $ (runReaderT errRd st)
        `catch` (\(e :: IOException) -> return ())
    x2 <- liftIO $ (runReaderT okRd st)
        `catch` (\(e :: IOException) -> return ())
    return ()

tst2 = do 
    st <- newIORef ([] :: [String])
    runReaderT runRdMain st
    res <- readIORef st
    return res
    
-- ["ok rd: ok","err rd: before read"]

    
        


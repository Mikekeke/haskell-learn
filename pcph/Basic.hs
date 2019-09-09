import Control.Concurrent
import Text.Printf
import Control.Monad
import System.IO

setReminder t = do
    printf "Ok, I'll remind you in %d seconds\n" t
    threadDelay (10^6 * t)
    printf "%d seconds is up! BING!\BEL\n" t

reminder = loop where 
    loop = do
        t <-  getLine
        if t == "exit" then return ()
        else (forkIO . setReminder . read $ t) >> loop

------------------------------------------------------------------------
delaySec = threadDelay . (10^6*)

unlocker mv = do
    delaySec 2 
    putStrLn "Unlock?"
    ans <- getLine
    if ans == "y" then putMVar mv ()
    else unlocker mv

lock = do
    hSetBuffering stdout NoBuffering
    mv <- newEmptyMVar
    forkIO $ unlocker mv
    putStrLn "Locked now"
    takeMVar mv >> putStrLn "Done"

-----------------------------------------------------------------------
type Stream a = MVar (Item a)
data Item a = Item a (Stream a)


data MChan a = MChan (MVar (Stream a)) (MVar (Stream a))

mNewChan :: IO (MChan a)
mNewChan = do
    hole <- newEmptyMVar
    readVar <- newMVar hole
    writeVar <- newMVar hole
    return $ MChan readVar writeVar

mWriteChan :: MChan a -> a -> IO ()
mWriteChan (MChan _ writeV) v = do
    newHole <- newEmptyMVar
    oldHole <- takeMVar writeV
    putMVar oldHole (Item v newHole)
    putMVar writeV newHole

readChan :: MChan a -> IO a
readChan (MChan readV _) = do
    stream <- takeMVar readV
    Item v rest <- takeMVar stream
    putMVar readV rest
    return v
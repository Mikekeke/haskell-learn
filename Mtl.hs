import Control.Monad.Reader
import Control.Monad.IO.Class
import System.Console.Haskeline
import Control.Monad.Except

m1 :: Monad m => ReaderT Int m String
m1 = undefined

ff :: ReaderT Int IO String
ff = do
    x1 <- m1
    x2 <- liftIO (const "x" <$> putStrLn "> ")
    undefined

ff2 x = do
    putStrLn "1"
    when (x > 3) $ putStrLn "2"
    putStrLn "3"

ff3' ::  Int -> ExceptT String IO String
ff3' x =  do 
    liftIO $ putStrLn "1"
    when (x > 3) $ throwError "err"
    liftIO $ putStrLn "2"
    return "end"

ff3 = runExceptT . ff3'

ff4 = do
    putStrLn "1"
    True <- return False
    putStrLn "2"

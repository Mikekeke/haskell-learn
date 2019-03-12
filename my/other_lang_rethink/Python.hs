
{- rethinking python fuckup situation in Haskell -}

{-# LANGUAGE GADTs #-}

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer

data Doc = EmptyDoc | CreatedDoc {num :: Int} | DownloadedDoc {num :: Int, dt :: String} deriving Show

reqNum :: Monad m => Int -> ExceptT String m Int
reqNum 1 = return 33
reqNum 2 = throwError "err num"
reqNum 3 = return 500

reqData :: Monad m => Int -> ExceptT String m String
reqData 2 = return "OK data"
reqData 1 = throwError "BAD data"

validateDoc :: Doc -> ExceptT String (State Doc) ()
validateDoc EmptyDoc = pure ()
validateDoc doc@(CreatedDoc n) | n < 100 = pure ()
                               | otherwise = throwError $ "invalid created doc: " ++ show doc
validateDoc doc@(DownloadedDoc n dt') | n < 100 && dt' == "OK data" = pure ()
                                     | otherwise = throwError $ "invalid DownloadedDoc doc: " ++ show doc

createDoc :: ExceptT String (State Doc) ()
createDoc =  do
    d <- get
    -- can do this stuff typesafe with smth like phantom types for Doc 
    -- w/o this runtrime check, but need indexed monad (what I found, m.b. some other ways awailable)
    -- so u can change type of state with "put" and "modify"
    case d of 
        EmptyDoc -> reqNum 1 >>= return . CreatedDoc >>= \doc -> validateDoc doc >> put doc
        o -> throwError $ "Cant use " ++ show o ++ " for creation"

createDoc2 :: ExceptT String (State Doc) ()
createDoc2 =  do
    d <- get
    case d of 
        EmptyDoc -> reqNum 3 >>= return . CreatedDoc >>= \doc -> validateDoc doc >> put doc
        o -> throwError $ "Cant use " ++ show o ++ " for creation"

dlDoc :: ExceptT String (State Doc) ()
dlDoc =  do
    d <- get
    case d of 
        (CreatedDoc num') -> reqData 1 >>= put . DownloadedDoc num'
        o -> throwError $ "Cant use " ++ show o ++ " for download"

tst1 = runState (runExceptT $ createDoc >> dlDoc) EmptyDoc
tst2 = runState (runExceptT $ createDoc >> dlDoc) (CreatedDoc 10)
tst3 = runState (runExceptT $ createDoc2 >> dlDoc) EmptyDoc


{- ************************************ -}
data Dat = Ok | Nok
data DosState = DocState {nm :: Maybe Int, dat :: Maybe Dat}

fetchN :: Monad m => Int -> ExceptT String m Int
fetchN 1 = return 1
fetchN 2 = return 10
fetchN 3 = return 10
fetchN _ = throwError "Fetch num connection err"

validateNum :: Monad m => Int -> ExceptT String m Int
validateNum 1 = return 1
validateNum _ = throwError "Bad num"

fetchD :: Monad m => Int -> ExceptT String m Dat
fetchD 1 = return Ok
fetchD 2 = return Ok
fetchD 3 = return Nok
fetchD _ = throwError "Fetch data connection err"

validateD :: Monad m => Dat -> ExceptT String m Dat
validateD Ok = return Ok
validateD _ = throwError "Bad data"



{- 
rethink: getting e.g. doc's num can fail 2 ways - "http" request and validation, so it should be 'ExceptT e2 (Either e1) a' or smth?
-}
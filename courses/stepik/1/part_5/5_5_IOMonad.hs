module Main where

import           Data.List
import           System.Directory

-- 1st task
main' :: IO ()
main' = do
    putStr "What is your name?\nName: "
    name <- getLine
    if not $ null name then putStrLn $ "Hi, " ++ name ++ "!" else main

--2nd task
deleteMessage = ("Removing file: "++)
deleteFun path = putStrLn (deleteMessage path) >> putStrLn "imitating \"removeFile\""
main = do
    putStr "Substring: "
    subs <- getLine
    case subs of
        [] -> putStrLn "Canceled"
        _ -> do
            files <- getDirectoryContents "." >>= return . filter (isInfixOf subs)
            mapM_ deleteFun files

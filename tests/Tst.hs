import           Control.Monad
import           Data.Char
import           Data.List
import           System.Directory
import           System.IO

deleteMessage = ("Removing file: "++)
deleteFun path = putStrLn (deleteMessage path) >> putStrLn "imitating \"removeFile\""
main' = do
    putStr "Substring: "
    subs <- getLine
    case subs of
        [] -> putStrLn "Canceled"
        _ -> do
            files <- getDirectoryContents "." >>= return . filter (isInfixOf subs)
            mapM_ deleteFun files

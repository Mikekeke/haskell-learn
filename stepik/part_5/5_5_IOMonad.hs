module Main where

main :: IO ()
main = do 
    putStr "What is your name?\nName: "
    name <- getLine
    if not $ null name then putStrLn $ "Hi, " ++ name ++ "!" else main
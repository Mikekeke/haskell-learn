import System.Exit

quotes :: [(Int, [Char])]
quotes = [
    (1, "Shit happens"),
    (2, "Everybody dies"),
    (3, "Price the Sun")
    ]

getQuote ("n":_) = []
getQuote (ix:rest) = maybe "Wrong input" id (lookup (read ix) quotes) : getQuote rest
getQuote rest = "Wrong input" : getQuote rest

main :: IO ()
main = do
    putStr "Print quote number 1 to 3\n"
    getContents >>= mapM_ putStrLn . getQuote . lines
    -- ws <- getContents
    -- mapM_ putStrLn (getQuote . lines $ ws)
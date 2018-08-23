import Control.Monad.State

isClosing :: Char -> Bool
isClosing = flip elem [')',']','}']

getOpeningFor c | c == ')' = '('
                | c == ']' = '['
                | c == '}' = '{'

newtype Stack a = Stack {getSt :: [a]} deriving Show


push :: a -> Stack a -> Stack a
push x s = Stack $ x : getSt s

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack []) = (Nothing, Stack [])
pop (Stack (x:xs)) = (Just x, Stack xs)

top :: Stack a -> Maybe a
top (Stack []) = Nothing
top (Stack (x:_)) = Just x

isMatchTop :: Char -> Stack Char -> Bool
isMatchTop c st = let res = (==) <$> top st <*> pure (getOpeningFor c)
                in maybe False id res

step n c st | isClosing c && isMatchTop c st = (n+1, snd (pop st))  
            | isClosing c && not (isMatchTop c st) = (n+1, snd (pop st))  
            | otherwise = (n+1, push c st) 

go :: Int -> Char -> State (Stack Char) Int
go n c = do
    modify (push c)
    return n

emptyStack = Stack []


-- checkStep :: Int -> Char -> State (Stack Char) Int
-- checkStep cnt c | isClosing c = undefined
--                 | otherwise = modify (push c) >> return $ cnt + 1

check :: String -> String
check = undefined



main :: IO ()
main = do
    input <- getLine
    putStrLn input
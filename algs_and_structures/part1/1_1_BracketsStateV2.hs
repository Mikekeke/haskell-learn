import           Control.Monad.State
import           Debug.Trace

isClosing :: Char -> Bool
isClosing = flip elem [')',']','}']

getOpeningFor c | c == ')' = '('
                | c == ']' = '['
                | c == '}' = '{'

newtype Stack a = Stack {getSt :: [a]} deriving (Show, Eq)
emptyStack = Stack []
isEmpty :: Eq a => Stack a -> Bool
isEmpty = (==) emptyStack

push :: a -> Stack a -> Stack a
push x s = Stack $ x : getSt s

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack [])     = (Nothing, Stack [])
pop (Stack (x:xs)) = (Just x, Stack xs)

top :: Stack a -> Maybe a
top (Stack [])    = Nothing
top (Stack (x:_)) = Just x

type Step = Int
type Brackets = String
type CheckState = (Step, Stack Char)
type Result = Either Int String
succRes = Right "Success"
isMatchTop :: Char -> Stack Char -> Bool
isMatchTop c st = let res = (==) <$> top st <*> pure (getOpeningFor c)
                in maybe False id res

check :: Char -> CheckState -> (Result, CheckState)
check b (step, stack) | isClosing b &&  matchingTop = (succRes, (succ step, snd (pop stack)))
                      | isClosing b && not matchingTop = (Left step, (step, stack))
                      | otherwise = (succRes, (succ step, push b stack))
                      where matchingTop = isMatchTop b stack

checkBr :: String -> State CheckState Result                                  
checkBr [] = return succRes
checkBr (b:bs) = do
    r <- state (check b)
    case r of 
        (Right _) -> checkBr bs
        other -> return other

main :: IO ()
main = do
    input <- getLine
    putStrLn input

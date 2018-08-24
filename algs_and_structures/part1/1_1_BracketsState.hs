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

type StepCnt = Either String Int
iniCnt :: StepCnt
iniCnt = Right 0

isMatchTop :: Char -> Stack Char -> Bool
isMatchTop c st = let res = (==) <$> top st <*> pure (getOpeningFor c)
                in maybe False id res

succStep :: StepCnt -> StepCnt
succStep = fmap (+1)
failStep :: StepCnt -> StepCnt
failStep = (=<<) (Left . show . (1 +))

checkStep :: Char -> StepCnt -> Stack Char -> (StepCnt, Stack Char)
checkStep c sc st | isClosing c && isMatchTop c st = ((+1) <$> sc, snd (pop st))
                  | isClosing c && not (isMatchTop c st) = (failStep sc, st)
                  | otherwise = ((+1) <$> sc, push c st)

check_ cs cnt stack | (c':cs') <- cs , (Right _) <- cnt = let (cnt', stack') = checkStep c' (trace (show cnt) cnt) stack
                                                          in check_ cs' cnt' stack'
                    | otherwise                         = (cnt, stack)

check :: String -> State (Stack Char) StepCnt
check brackets = state $ check_ brackets iniCnt

stateStep :: StepCnt -> Char -> State (Stack Char) StepCnt
stateStep sc c = state $ checkStep c sc
-- check2 :: StepCnt -> String -> State [Stack Char] StepCnt
-- check2 ini st = do
--     c <- st
--     cnt1 <- state $

tick :: State Int Int
tick = do
    n <- get
    put (n+1)
    return n
-- !!!!!!!!!!!!!!!!!!
go :: Int -> Char -> State (Stack Char) Int
go n c = modify (push c) >> return (n+1)
-- go n c = modify (push c) >> return $ n+1 -- почему так ошибка??
-- !!!!!!!!!!!!!!!!!!

main :: IO ()
main = do
    input <- getLine
    putStrLn input

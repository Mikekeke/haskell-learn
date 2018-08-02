import           Control.Monad.State
import           Data.List
import           Data.Monoid
import           Debug.Trace

find1 p []     = Nothing
find1 p (x:xs) = if p x then Just x else find1 p xs

find2 :: (a -> Bool) -> [a] -> Maybe a
find2 p = foldr (\x b -> if p x then Just x else b) Nothing

f1 :: State [Char] Int
f1 =  get >> modify (++ "FF") >> return 4
f2 :: State [Char] Int
f2 =  get >>= \s -> return (4 + (length s))
-- f =  do
--     s1 <- get
--     modify (++ "F")
--     return 4


data Fromwhere = Head | Tail | End deriving Show
type ListSt a = (Fromwhere, [a])

-- ver .1
delFromHT :: ListSt a -> ([a], ListSt a)
delFromHT (_, [])    = ([], (End, []))
delFromHT (Head, xs) = ([head xs], (Tail, drop 1 xs))
delFromHT (Tail, xs) = ([last xs], (Head, init xs))

newState1 :: State (ListSt a) [a]
newState1 = state delFromHT

calcShit :: Show a => [a] -> State (ListSt a) [a]
calcShit l = do
   l' <- newState1
   case null l' of
    True -> (return l)
    _    -> calcShit (l' ++ l)
-- λ: runState (calcShit "z") (Head, "123456")
-- ("435261z",(End,""))

-- ver. 2
delFromHT' :: ListSt a -> ListSt a
delFromHT' (_, [])    =(End, [])
delFromHT' (Head, xs) =(Tail, drop 1 xs)
delFromHT' (Tail, xs) = (Head, init xs)

resFromHT (_, [])    = []
resFromHT (Head, xs) = [head xs]
resFromHT (Tail, xs) = [last xs]

newState2 :: State (ListSt a) [a]
newState2 = do
    s <- get
    modify delFromHT'
    return $ resFromHT s

calcShit2 ::  Show a => [a] -> State (ListSt a) [a]
calcShit2 l = do
   l' <- newState2
   case null l' of
    True -> return l
    _    -> calcShit (l ++ l')

fnFind _ []     = Nothing
fnFind p (x:xs) = case p x of True -> Just x; _ -> fnFind p xs

stFn :: (a -> Bool) -> a -> State [a] (Maybe a)
stFn p x = case p x of
    True  -> return (Just x)
    False -> modify (x:) >> return Nothing

fn :: (a -> Bool) -> [a] -> State [a] (Maybe a)
fn _ [] = modify reverse >> return Nothing
fn p (x:xs) = do
    x' <- stFn p x
    case x' of
        Just _  -> modify reverse >> return x'
        Nothing -> fn p xs



{-# LANGUAGE ViewPatterns #-}
import Control.Monad (foldM)
import Debug.Trace


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




-- isBalanced' xs = maybe False null $ foldM op [] xs
--   where
--     op ('(':xs) ')'          = Just xs
--     op ('[':xs) ']'          = Just xs
--     op ('{':xs) '}'          = Just xs
--     op xs x | x `elem` ")]}" = Nothing
--             | otherwise      = Just (x:xs)

type Result = Either Int String
ini = (0, emptyStack)
resultCheck (cnt, s) = if isEmpty s then "Success" else show cnt
isClosing :: Char -> Bool
isClosing = flip elem [')',']','}']

check bs = either (show :: Int -> String) (resultCheck) $ foldM f ini bs
        where
            f (cnt, stack) bracket  = case flip (,) bracket <$> top stack of
                Just ('(',')')              -> Right(succ cnt, snd $ pop stack)
                Just ('[',']')              -> Right(succ cnt, snd $ pop stack)
                Just ('{','}')              -> Right(succ cnt, snd $ pop stack)
                Just (isClosing -> False,_) -> Right (succ cnt, push bracket stack)
                Nothing                     -> Right (succ cnt, push bracket stack)
                _                           -> Left (succ cnt) 


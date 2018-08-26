{-# LANGUAGE ViewPatterns #-}
import Control.Monad (foldM)
import Data.Char
import Debug.Trace
import Data.Maybe


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
topUnsafe = fromJust . top

size :: Stack a -> Int
size = length . getSt




-- isBalanced' xs = maybe False null $ foldM op [] xs
--   where
--     op ('(':xs) ')'          = Just xs
--     op ('[':xs) ']'          = Just xs
--     op ('{':xs) '}'          = Just xs
--     op xs x | x `elem` ")]}" = Nothing
--             | otherwise      = Just (x:xs)

type Result = Either Int String
ini = (0, emptyStack)
resultCheck (cnt, s) = if isEmpty s then "Success" else show $ size s
closing = [')',']','}']
opening = ['(','[','{']
brackets = closing ++ opening
isClosing :: Char -> Bool
isClosing = flip elem closing
isOpening = not . isClosing
isValidPair = flip elem [('(',')') , ('[',']'), ('{','}') ]


-- check bs = either show (resultCheck) $ foldM f ini bs
check bs = either (show :: Int -> String) (resultCheck) $ foldM f ini bs
        where
            f (cnt, stack) b | not (elem b brackets) = Right(succ cnt, stack)
                             | isEmpty stack && isClosing b = Left (succ cnt) 
                             | isEmpty stack || isOpening b  = Right (succ cnt, push b stack)
                             | isValidPair (topUnsafe stack, b) = Right(succ cnt, snd $ pop stack)
                             | otherwise = Left (succ cnt)

testInputs = ["([](){([])})", "()[]}", "{{[()]]"]
testOuts = ["Success", "5", "7"]
test = zipWith (==) testOuts (map check testInputs)

check "() ( {}" надо 3, получается 1

main :: IO ()
main = getLine >>= putStrLn . check
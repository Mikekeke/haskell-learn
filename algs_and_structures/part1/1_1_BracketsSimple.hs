import           Control.Monad (foldM)
import           Data.Maybe

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

ini = (0, emptyStack)
resultCheck (_, stack) = if isEmpty stack then "Success" else show $ snd (topUnsafe stack)
closing = [')',']','}']
opening = ['(','[','{']
brackets = closing ++ opening
isClosing :: Char -> Bool
isClosing = flip elem closing
isOpening = not . isClosing
isValidPair = flip elem [('(',')') , ('[',']'), ('{','}')]

check bs = either (show :: Int -> String) (resultCheck) $ foldM f ini bs
        where
            f (cnt, stack) b | not (elem b brackets) = Right(stepNum, stack)
                             | isEmpty stack && isClosing b = Left stepNum
                             | isEmpty stack || isOpening b  = Right (stepNum, push (b, stepNum) stack)
                             | isValidPair (fst . topUnsafe $ stack, b) = Right(stepNum, snd $ pop stack)
                             | otherwise = Left stepNum
                             where stepNum = succ cnt

testInputs = ["([](){([])})", "()[]}", "{{[()]]"]
testOuts = ["Success", "5", "7"]
test = zipWith (==) testOuts (map check testInputs)

main :: IO ()
main = check <$> getLine >>= putStrLn 
-- main = getLine >>= putStrLn . check


-- short one from solutions
-- isCorrect [] [] = "Success"
-- isCorrect s  [] = show . fst . last $ s
-- isCorrect s (e@(i, c) : l)
--   | c `elem` "([{" = isCorrect (e : s) l
--   | c `elem` ")]}" = case s of
--     []            -> show i
--     ((_, t) : s') -> if Just t == lookup c [(')', '('), (']', '['), ('}', '{')]
--                      then isCorrect s' l
--                      else show i
--   | otherwise      = isCorrect s l

-- main = isCorrect [] . zip [1 ..] <$> getLine >>= putStrLn
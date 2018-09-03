import           Debug.Trace
import Control.Applicative ((<$>), (<*>))
import Data.List (elemIndex, sortBy)
import Data.Maybe (fromMaybe)
import Data.Char (isDigit)
import Data.Ord (comparing)

fact x = product [1..x]

f x
    | x == 25 = 2
    | x `mod` 5 == 0 = 1
    | otherwise = 0

zeros :: Int -> Int
zeros x = foldl (\b a -> b + (f a))  0  [1..x]

-- String repeat --
repeatStr :: Int -> String -> String
repeatStr n str = mconcat $ replicate n str
-- cool effect'ish from solutions 
-- repeatStr n str = [1..n] >> str

-- Equal Sides Of An Array --
-- https://www.codewars.com/kata/5679aa472b8f57fb8c000047/train/haskell
findEvenIndex :: [Int] -> Int
findEvenIndex [] = 0
findEvenIndex [_] = 0
findEvenIndex (x:xs) | sum xs == 0 = 0
                     | otherwise = let iniL = x
                                       ln = length xs
                                       iniR = sum . tail $ xs
                                       go l r cnt (x':xs') | cnt > ln = -1
                                                           | l == r = cnt
                                                           | null xs' = -1
                                                           | otherwise = go (l+x') (r - head xs') (succ cnt) xs'
                                    in go iniL iniR 1 xs
-- best answer, much faster and less memory than mine
findEvenIndex1 :: [Int] -> Int
findEvenIndex1 = fromMaybe (-1) . elemIndex True .
  (zipWith (==) <$> scanl1 (+) <*> scanr1 (+))

-- yourOrderPlease -- 
{- sorts string like "f2 gg0 dd1" by digit inside word-}
pullNum s = head . dropWhile (not . isDigit) $ s
yourOrderPlease :: String -> String
yourOrderPlease = unwords . sortBy (comparing pullNum) . words
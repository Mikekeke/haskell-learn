import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Data.Char
import Control.Monad

strings = ["abc", "defg", "hij"]
logFirstAndRetSecond :: WriterT String (Reader [String]) String
logFirstAndRetSecond = do
    el1 <- lift $ asks head
    el2 <- lift $ asks (map toUpper . head . tail)
    tell el1
    return el2

tst1 = runReader (runWriterT logFirstAndRetSecond) strings

separate :: (a -> Bool) -> (a -> Bool) -> [a] -> WriterT [a] (Writer [a]) [a]
separate p1 p2 xs = do
    tell $ filter p1 xs
    lift $ tell $ filter p2 xs
    return $ filter (\x -> not $ p1 x || p2 x) xs

separate' :: (a -> Bool) -> (a -> Bool) -> [a] -> WriterT [a] (Writer [a]) [a]
separate' _ _ [] = return []
separate' p1 p2 (x:xs) = do
    when (p1 x) $ tell [x]
    when (p2 x) $ lift $ tell [x]
    ys <- separate p1 p2 xs
    return $ case (p1 x) || (p2 x) of
        True -> ys
        False -> x : ys

{-
λ: :t or
or :: Foldable t => t Bool -> Bool
λ: mfilter (not.or.sequenceA[(>2), (<4)]) $ [3]
[]
λ: mfilter (or.sequenceA[(>2), (<4)]) $ [3]
[3]
λ: mfilter (or.sequenceA[(>2), (<4)]) $ [4]
[4]
λ: mfilter (and.sequenceA[(>2), (<4)]) $ [4]
[]
λ: mfilter (and.sequenceA[(>2), (<4)]) $ [2]
[]
λ: mfilter (and.sequenceA[(>2), (<4)]) $ [3]

import           Data.Maybe           (catMaybes)

separate :: (a -> Bool) -> (a -> Bool) -> [a] -> WriterT [a] (Writer [a]) [a]
separate p1 p2 = fmap catMaybes. mapM analyze where
    analyze x = do
      when (p1 x) $ tell [x]
      when (p2 x) $ lift $ tell [x]
      return$ mfilter (not.or.sequenceA[p1,p2]) $ Just x

-}
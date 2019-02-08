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

traverse id [(\x -> x + 2)]
foldr (\a b -> (:) <$> id a <*> b) (pure []) [(\x -> x + 2)]

(\a b -> (:) <$> id a <*> b) (\x -> x + 2) $ foldr (\a b -> (:) <$> id a <*> b) (pure []) []
(\a b -> (:) <$> id a <*> b) (\x -> x + 2) (pure [])
(:) <$> id (\x -> x + 2) <*> (pure [])
(:) <$> (\x -> x + 2) <*> (pure [])
(:) . (\x -> x + 2) <*> (pure [])
(:) . (\x -> x + 2) <*> (\ -> [])

*******
instance Applicative ((->) t) where
  pure ::
    a
    -> ((->) t a)
  pure x = \_ -> x
  (<*>) ::
    ((->) t (a -> b))
    -> ((->) t a)
    -> ((->) t b)
  (<*>) fa f = \e -> fa e $ f e
*******
\y -> ((:) . (\x -> x + 2)) y $ (\ -> []) y
\y -> ((y+2):) []
\y -> (y+2) : []


-}
ff :: Num a => (a -> [a]) -> a -> [a]
ff v = (:) <$> (\x -> x + 2) <*> v
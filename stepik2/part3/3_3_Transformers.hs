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

-- **********************************************
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
-- **********************************************


type MyRW = ReaderT [String] (Writer String)
logFirstAndRetSecond' :: MyRW String
logFirstAndRetSecond' = do
  el1 <- myAsks head
  el2 <- myAsks (map toUpper . head . tail)
  myTell el1
  return el2

myAsks :: ([String] -> a) -> MyRW a
myAsks = asks

myTell :: String -> MyRW ()
myTell = lift . tell

-- *****************************************

type MyRWT m = ReaderT [String] (WriterT String m)

runMyRWT :: MyRWT m a -> [String] -> m (a, String)
runMyRWT rwt s = runWriterT (runReaderT rwt s)
-- runMyRWT = (runWriterT .). runReaderT

myAsks' :: Monad m => (r -> a) -> ReaderT r m a
myAsks' = asks

myTell' :: Monad m => String -> ReaderT [String] (WriterT String m) ()
myTell' = lift . tell

myLift :: Monad m => m a -> ReaderT [String] (WriterT String m) a
myLift = lift . lift

logFirstAndRetSecond'' :: MyRWT IO String
logFirstAndRetSecond'' = do
  el1 <- myAsks' head
  myLift $ putStrLn $ "First is " ++ show el1
  el2 <- myAsks' (map toUpper . head . tail)
  myLift $ putStrLn $ "Second is " ++ show el2
  myTell' el1
  return el2
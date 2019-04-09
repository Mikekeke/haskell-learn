import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Data.Char
import Control.Monad
import Data.List
import Data.Bifunctor

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

!to-understand
sequenceA == traverse id
traverse id [(\x -> x + 2)]
foldr (\a b -> (:) <$> id a <*> b) (pure []) [(\x -> x + 2)]

(\a b -> (:) <$> id a <*> b) (\x -> x + 2) $ foldr (\a b -> (:) <$> id a <*> b) (pure []) []
(\a b -> (:) <$> id a <*> b) (\x -> x + 2) (pure [])
(:) <$> id (\x -> x + 2) <*> (pure [])
(:) <$> (\x -> x + 2) <*> (pure [])
(:) . (\x -> x + 2) <*> (pure [])
(:) . (\x -> x + 2) <*> (\_-> [])
((:) . (\x -> x + 2) <*> (\_-> [])) :: Num a => a -> [a]

λ: ((:) . (\x -> x + 2) <*> (\_-> [])) 2
[4]

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

myAsk = myAsks id

myTell :: String -> MyRW ()
myTell = lift . tell

-- *****************************************

type MyRWT m = ReaderT [String] (WriterT String m)

runMyRWT :: MyRWT m a -> [String] -> m (a, String)
runMyRWT rwt s = runWriterT (runReaderT rwt s)
-- runMyRWT = (runWriterT .). runReaderT

myAsks' :: Monad m => (r -> a) -> ReaderT r m a
myAsks' = asks

myAsk' = myAsks' id



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


logFirstAndRetSecondSafe :: MyRWT Maybe String
logFirstAndRetSecondSafe = do
  xs <- myAsk'
  case xs of
    (el1 : el2 : _) -> myTell' el1 >> return (map toUpper el2)
    _ -> myLift Nothing

veryComplexComputation :: MyRWT Maybe (String, String)
veryComplexComputation = do
    (odds, evens) <- myAsks' (partition (odd.length))
    case (odds, evens) of
        (o1:o2:_, e1:e2:_) -> do
            myTell' $ e1 ++ ","
            myTell' o1 
            return $ join bimap (map toUpper) (e2, o2)
        _ -> myLift Nothing

-- bertter after seing solutions (but could grasp myself, doh)
veryComplexComputation' :: MyRWT Maybe (String, String)
veryComplexComputation' = do
    (o1:o2:_, e1:e2:_) <- myAsks' (partition (odd.length))
    myTell' $ e1 ++ "," ++ o1 
    return $ join bimap (map toUpper) (e2, o2) -- see BimapJoin.hs

{- from solutions
magic arrow stuff

import Control.Arrow

veryComplexComputation :: MyRWT Maybe (String, String)
veryComplexComputation = do
    (e1 : e2 : _, o1 : o2 : _) <- myAsks $ filter (even . length) &&& filter (odd . length)
    myTell $ e1 ++ "," ++ o1
    return $ (map toUpper e2, map toUpper o2)
**********************************

stuffs solution (like using "local")

veryComplexComputation :: MyRWT Maybe (String, String)
veryComplexComputation = do
  s1 <- myWithReader (filter $ even . length) logFirstAndRetSecond
  myTell ","
  s2 <- myWithReader (filter $ odd  . length) logFirstAndRetSecond
  return (s1, s2)

myWithReader :: Monad m => ([String] -> [String]) -> MyRWT m a -> MyRWT m a
myWithReader = withReaderT

-}

-- GHCi> runMyRWT veryComplexComputation ["abc","defg","hij"]
-- Nothing
-- GHCi> runMyRWT veryComplexComputation ["abc","defg","hij","kl"]
-- Just (("KL","HIJ"),"defg,abc")








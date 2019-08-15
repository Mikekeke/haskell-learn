import Control.Applicative
import Data.List
import Data.Char
import Debug.Trace

dec l n = 
    let 
        lss = replicate n l
        go :: [[a]] -> [[a]]
        go [] = []
        go (x:xs) = undefined
    in undefined

ls = [0,1,2]
fn = do
    a <- ls
    b <- ls
    c <- ls
    return [a,b,c]

sqs :: [[a]] -> [[a]]
-- sqs [] = [[]]
-- sqs (x:xs) = (:) <$> x <*> sqs xs
sqs = foldr g (pure [])
    where g x1 = (<*>) $ (:) <$> x1

fn2 n = sequence $ replicate n ls

a = [[1,2],[3,4]]

-- Applicative will be enough here
findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = pure Nothing
findM k (x:xs) = f <$> (k x) <*> (findM k xs) where 
    f x' = if x' then const (Just x) else id

findM' :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM' k l = foldr (\a b -> f a <$> k a <*> b) (pure Nothing) l where
    f v x' = if x' then const (Just v) else id

findM'' :: (Monad m, Show a) => (a -> m Bool) -> [a] -> m (Maybe a)
findM'' k l = foldr (\a -> liftA2 (f a) (k a)) (pure Nothing) l where
    f v x' = if x' then const (Just v) else id

fs :: (Monad m, Show a) => [(a -> m Bool) -> [a] -> m (Maybe a)]
fs = [findM, findM', findM'']
tst = (sequence . sequence fs) (Just . isUpper) "teST"

{- https://hackage.haskell.org/package/extra-1.6.17/docs/src/Control.Monad.Extra.html#findM
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do b <- b; if b then t else f

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM p [] = return Nothing
findM p (x:xs) = ifM (p x) (return $ Just x) (findM p xs)
-}
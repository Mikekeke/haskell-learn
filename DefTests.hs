
-- {-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE ScopedTypeVariables #-} -- for types in lambdas
{-# LANGUAGE RecordWildCards  #-} -- for types in lambdas

import System.IO
import Data.IORef
import Control.Monad.Writer hiding (fix)
import Control.Monad.Except hiding (fix)
import Control.Monad.State hiding (fix)
import Control.Monad.Reader hiding (fix)
import Control.Exception as Ex
import Control.Applicative
import Data.Monoid
import Debug.Trace
import qualified Data.List as L

type Entry = ([Char], ([Char], [Integer]))
-- ex :: [Entry]
ex = [("a", [("a11", [1,2]), ("a12", [3,4])])]

f :: Monoid b => [(a,b)] -> b
f = mconcat . map snd

-- append :: Entry -> [Entry] -> [Entry]
-- append en ens = 


fix f = let x = f x in x
fn = \rec' x -> if (x < 0) then rec' (x + 3) else x
test1 = fix fn (-10)



incRef :: IORef Int -> IO ()
incRef var = do
    val <- readIORef var
    writeIORef var (val+1)

type App1 = ExceptT String (StateT String IO) ()

run1 = runStateT (runExceptT (action `catchError` hErr)) "|" 
    where 
        action :: App1
        action = do
            modify ('a':)
            error "err"
            modify ('b':)
        hErr :: String -> App1
        hErr e = do
            modify ('E':)

data TestException = TestException deriving Show
instance Exception TestException

brokenRead = readFile "not_existed.txt"

type AppLoseState = StateT [String] IO ()

errSt :: AppLoseState
errSt = do
        modify ("err st: before read" : )
        s <- liftIO brokenRead
        modify (s:)
        modify ("err st: after read" : )

okSt :: AppLoseState
okSt = modify ("ok state: ok" : )

        

runErrMain :: AppLoseState     
runErrMain = do
    s <- get
    (r1, s1) <- liftIO $ (runStateT errSt s)
        -- no way to get state from failed "errSt"
        `catch` (\(e :: IOException) -> return ((), s))
    put s1
    (r2, s2) <- liftIO $ (runStateT okSt s1)
        `catch` (\(e :: IOException) -> return ((), s1))
    put s2
    modify ("END" :)

tst1 = runStateT runErrMain [] -- ((),["END","ok state: ok"])

type AppNotLoseState = ReaderT (IORef [String]) IO ()

errRd :: AppNotLoseState
errRd = do
    var <- ask
    liftIO $  modifyIORef var ("err rd: before read" :)
    liftIO brokenRead
    liftIO $  modifyIORef var ("err rd: after read" : )

okRd :: AppNotLoseState
-- okRd = ask >>= \var -> liftIO $ modifyIORef var ("ok rd: ok" : )
okRd = ask >>= liftIO . flip modifyIORef ("ok rd: ok" :)

runRdMain :: AppNotLoseState     
runRdMain = do
    -- st <- liftIO $ 
    st <- ask
    x1 <- liftIO $ (runReaderT errRd st)
        `catch` (\(e :: IOException) -> return ())
    x2 <- liftIO $ (runReaderT okRd st)
        `catch` (\(e :: IOException) -> return ())
    return ()

tst2 = do 
    st <- newIORef ([] :: [String])
    runReaderT runRdMain st
    res <- readIORef st
    return res
    
-- ["ok rd: ok","err rd: before read"]

trav :: Applicative f => (a -> f b) -> [a] -> f [b]
trav _ [] = pure []  
trav k (x:xs) = (:) <$> k x <*> trav k xs 

seqs :: Applicative f => [f a] -> f [a]
seqs [] = pure []
seqs (x:xs) = (:) <$> x <*> seqs xs

seqs' :: Applicative f => [f a] -> f [a]
seqs' = traverse id

trav' :: Applicative f => (a -> f b) -> [a] -> f [b]
trav' k' l = foldr (\a b -> (:) <$> k' a <*> b) (pure []) l
-- trav' k' l = foldr (liftA2 (:) . k') (pure []) l


tst3 m = foldMap m $ [(Just "1"),  undefined,  traceShowId (Just "2")]
ff 1 = "1"
ff undefined = "2"
{-
λ: ff 3
"2"
-}

type RW = Int
type EO a = RW -> (a, RW)

bind :: EO a -> (a -> EO b) -> EO b
bind m k = \rw -> let (a1,rw1) = m rw in (k a1) rw1
-- bind m k rw = let (a1,rw1) = m rw in (k a1) rw1
-- bind m k = \rw -> let (a1,rw1) = m rw
--                       (a2, rw2) = k a1 rw1
--                   in (a2, rw2)
                      


data Tree a = Nil | Branch (Tree a) a (Tree a) deriving Eq
instance Show a => Show (Tree a) where
    show Nil = "."
    show (Branch l x r) = "(" ++ show l ++ "(" ++ show x ++ ")" ++ show r ++ ")"
emptyTree = Nil

insert :: Ord a => a -> Tree a -> Tree a
insert a Nil = Branch Nil a Nil
insert a br@(Branch l x r) | a == x = br
                           | a < x = (Branch l x (insert a r))
                           | a > x = (Branch (insert a l) x r)

delete :: Ord a => a -> Tree a -> Tree a
delete _ Nil = Nil
delete a (Branch l x r) | a == x = undefined
                        | a < x = (Branch l x (delete a r))
                        | a > x = (Branch (delete a l) x r)
                        
r1 = insert 10 . insert 5 . insert 20 . insert 11  


fr f z [] = z
fr f z (x:xs) = f x (fr f z xs)

fl f z [] = z
fl f z (x:xs) = fl f (f x z) xs




rev xs = foldr (\x k -> \acc -> k (x:acc)) id xs []
{-
foldr (\x k -> \acc -> k (x:acc)) id [1,2] $ []
(\x k -> \acc -> k (x:acc)) 1 $ foldr (\x k -> \acc -> k (x:acc)) id [2] $ []
(\x k -> \acc -> k (x:acc)) 1 $ (\x k -> \acc -> k (x:acc)) 2 $ foldr (\x k -> \acc -> k (x:acc)) id [] $ []
(\x k -> \acc -> k (x:acc)) 1 $ (\x k -> \acc -> k (x:acc)) 2 $ id $ []
(\x k -> \acc -> k (x:acc)) 1 $ (\acc' -> id (2:acc')) $ []
(\x k -> \acc -> k (x:acc)) 1 $ (\acc' -> 2:acc') $ []
(\x k -> \acc -> k (x:acc)) 1 $ (2:) $ []
(\acc -> (2:(1:acc))) $ []
(2:(1:[])))
-}

-- pickFun x | even x = x / 2
--           | odd x = x * 3 + 1

-- collaz x | x == 1 = 1
--        | otherwise = step $ (pickFun x) 

data Player = Player {cards :: [Int]} deriving Show
noCards = Player []
lss = [noCards, noCards]

nums :: [Int]
nums = [1..10]
putCard Player {..} c = Player {cards = c:cards, ..}

spread players nums = let
    playersCount = length players
    go _ ps [] = ps
    go n ps cs = let (curr, rest) = splitAt n cs in go n (zipWith putCard ps curr) rest
    in go playersCount (cycle players) nums

caseFun s = case s of 
    _ | length s > 1 -> "ok"
      | otherwise -> "nok"
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}


import Control.Monad
import Debug.Trace
import Control.Monad


tk :: (a -> Bool) -> [a] -> [a]
tk p l = foldr (\x b -> (f x) b) [] l where
    f a = if p a then trace "call" (a:) else  const []

{-
tk (==1) [1,2]
-}

data Race = Black | White deriving (Show, Eq, Ord)
data Gender = Female | Male deriving (Show, Eq, Ord)

data Person (a::Race) = Person String deriving Show

p1 :: Person (White)
p1 = Person "Bob"

p2 :: Person Black
p2 = Person "Bob"

data Person2 (a::Race) where
    Person21 :: Int -> Person2 White
    Person22 :: String -> Person2 Black

data Person3 :: Race -> * where
    Person31 :: Int -> Person3 White
    Person32 :: String -> Person3 Black


-- p3 :: Person Male
-- p3 = Person "Bob"

data Smth = SmthA | SmthB

data Ex1 (a::Smth) where
    Ex1 :: Int -> Ex1 'SmthA

data Ex2 :: Smth -> * where
    Ex2 :: Int -> Ex2 'SmthA

data DomErr = PercentOverflow String deriving Show
checkBound :: String -> Int -> Either DomErr ()
checkBound name b = if b < 100 then Right () else Left $ PercentOverflow name


expand :: String -> [(Int, String)] -> (Int -> String)
expand name l = \a -> go a 0 l where
    go :: Int -> Int -> [(Int, String)] -> String
    go _ _ [] = name
    go x lo ((p,n):xs) = let hi = lo + p in 
        if x >= lo && x < hi then n else go x hi xs 

expand2 :: String -> [(Int, String)] -> Either DomErr (Int -> String)
expand2 name l = undefined where
    checkP x lo = if x+lo <= 100 then Right (x+lo) else Left (PercentOverflow name)
    k = checkP 10 >=> checkP 30 
    -- check = foldr (undefined) (Right 0) l

checkP :: Int -> Int -> Either DomErr Int    
checkP x lo = if x+lo <= 100 then trace "r" $ Right (x+lo) else trace "l" $ Left (PercentOverflow "ERR")

tst :: [Int] -> Either DomErr Int 
-- tst ls = foldr (\a b -> (checkP a) >=> b) pure ls 0
tst ls = foldr ((>=>).(checkP)) pure ls 0

{-
λ: tst $ take 100 [10,30..]
r
r
r
l
Left (PercentOverflow "ERR")
-}

td :: [(Int, String)]        
td = [(20, "A"), (30, "B"), (30, "C")]
td2 :: [(Int, String)]        
td2 = [(20, "A"), (30, "B"), (30, "C"), (100, "LOL")]

go1 :: Int -> Int -> [(Int, String)] -> Either DomErr String
go1 _ _ [] = Right "DEF"
go1 x lo ((p,n):xs) = let hi = lo + p in 
    (checkBound "DEF" hi) >> if x >= lo && x < hi then Right n else go1 x hi xs

-- f x = if x >= 0 && x < 3 then "a" 
--     else if x >= 3 && and x < 8 then "b"
--     else if x >= 8 && x < 10 then "c"

f0 :: Monad m => (a -> m b) -> m (a -> b)
f0 g = undefined where
    g1 =  g
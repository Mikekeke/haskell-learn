{-# LANGUAGE ScopedTypeVariables #-}

import Data.List
import Control.Monad

newtype Dp a b = Dp {runDp :: [a] -> Either String (b,[a])}
parseS :: Dp String b -> [String] -> Either String b
-- parseS p a = fst <$> (runDp p a) 
parseS = ((fst <$>) .) . runDp 

instance Functor (Dp a) where
    fmap = liftM

instance Applicative (Dp a) where
    pure = return
    (<*>) = ap

instance Monad (Dp a) where
    return a = Dp $ \l -> Right (a,l)
    dp >>= k = Dp $ \l ->  do
        (a', l') <- runDp dp l
        runDp (k a') l'

data Address = Address {street :: String, hn :: Int} deriving Show
data Person = Person {name :: String, addr :: Address} deriving Show

{- initial version
 dpDefault :: forall a. (String -> Either String a) -> Dp String a
 dpDefault f = Dp $ \ l -> case null l of
     True -> Left "empty"
     False -> 
         let 
             r :: Either String a
             r = f (head l) 
         in (,) <$> r <*> pure (tail l)
-}

-- shorter than initial (and don't need forall)
dpDefault :: (String -> Either String a) -> Dp String a
dpDefault f = Dp $ \ l -> case null l of
    True -> Left "empty"
    False -> (,) <$> f (head l) <*> pure (tail l)


dpString :: Dp String String
dpString = dpDefault Right

dpInt :: Dp String Int
dpInt = dpDefault $ res where
    res = \ v -> let parsed = (reads v :: [(Int, String)]) in
        case null parsed of
            True -> Left $ "can't parse int from: " ++ show v
            False -> Right . fst . head $ parsed

dpAddress :: Dp String Address
dpAddress = Address <$> dpString <*> dpInt 

dpPerson :: Dp String Person
dpPerson = Person <$> dpString <*> dpAddress

test = parseS dpPerson ["Bob", "Hell", "666"]
testBad = parseS dpPerson ["Bob", "Hell", "f666"]

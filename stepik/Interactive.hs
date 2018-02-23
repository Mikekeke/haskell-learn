module Interactive where

import Control.Applicative

data Cat = Cat String deriving Show

data Stalin = Stalin deriving Show
data Pff = Pff deriving Show




class Interbl a where
    talk::a -> String
    kick::a -> String

instance Interbl Cat where
    talk (Cat name) = name ++ " says mew"
    kick (Cat name) = name ++ " says MEEEW!!!"

instance Interbl Stalin where
    talk _ = "<Yoseef smokes>"
    kick _ = "RASSTRELYAT!!!"



-- START - making applicative
data MonCat a = MonCat a deriving Show

instance (Show a) => Interbl (MonCat a) where
    talk (MonCat x) = (show x) ++ " says mew"
    kick (MonCat x) = (show x) ++ " says MEEEW!!!"

instance Functor MonCat where
    fmap f (MonCat name) = MonCat (f name)

instance Applicative MonCat where
    pure = MonCat
    (MonCat f) <*> something = fmap f something
-- END - making applicative
--can call
-- fmap (++ " lol") (MonCat "Bob") = MonCat "Bob lol"
-- (MonCat(++ " lol")) <*> (pure "Bob") = MonCat "Bob lol"
-- fmap (\x->[x+2, x+3]) (MonCat 44) = MonCat [46,47]


allReactions :: (Interbl a) => a -> [String]
allReactions a = [f a | f <- [talk, kick]]

allReactions' :: (Interbl a) => a -> [String]
allReactions' c = [talk, kick] <*> pure c

allReactions'' :: (Interbl a) => a -> [String]
allReactions'' c = map (\f -> f(c)) [talk, kick]

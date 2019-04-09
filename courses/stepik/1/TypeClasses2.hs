module TypeClasses2 where

class Notated a where
    notation :: a -> String
    notation _ = "empty notation"

class DeNotated a where
    denotation :: a -> String
    denotation _ = "empty denot"

class (Notated a, DeNotated a) => Combined a where
    comb :: a -> String
    comb a = (denotation a) ++ " " ++ (notation a)

instance Notated Bool -- без этих интсансов не работает
instance DeNotated Bool -- без этих интсансов не работает
instance Combined Bool where

-- more experiments
class Show a => Printable a where
    prt :: a -> String

newtype Lol = Lol Int deriving Show
newtype Lol2 = Lol2 String deriving Show
newtype Lol3 a = Lol3 a deriving Show

instance Printable Lol where
    prt (Lol x) = "its num " ++ show x

instance Printable Lol2 where
    prt (Lol2 x) = "its num " ++ show x

class Addable a where
    add :: a -> a -> a

instance Addable Lol where
    add (Lol x) (Lol y) = Lol $ x+y

instance Num a => Addable (Lol3 a) where
    add (Lol3 x) (Lol3 y) = Lol3 $ x+y

test1 :: (Printable a, Addable a) => a -> a -> String
test1 a b  = prt $ add a b
res1 = test1 (Lol 4) (Lol 5)
-- res2 = test1 (Lol2 4) (Lol2 5) -- no instance
-- res3 = test1 (Lol3 4) (Lol3 5) -- no instance

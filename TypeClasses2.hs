module TypeClasses2 where

class Notated a where
    notation :: a -> String
    notation _ = "empty notation"

class DeNotated a where
    denotation :: a -> String
    denotation _ = "empty denot"

class (Notated a, DeNotated a) => Combined a where

instance Notated Bool -- без этих интсансов не работает
instance DeNotated Bool -- без этих интсансов не работает
instance Combined Bool

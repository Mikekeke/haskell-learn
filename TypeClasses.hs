module TypeClasses where


class Printable a where
  toString :: a -> [Char]
  toString _ = "unit type"

instance Printable Bool where
  toString True = "true"
  toString _ = "false"

instance Printable () where

instance (Printable a, Printable b) => Printable (a, b) where
  toString (a, b) = "(" ++ toString a ++ "," ++ toString b ++ ")"
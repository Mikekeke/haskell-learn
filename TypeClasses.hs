module TypeClasses where


class Printable a where
  toString :: a -> [Char]
  toString _ = "unit type"

instance Printable Bool where
  toString True = "true"
  toString _ = "false"

instance Printable ()

instance (Printable a, Printable b) => Printable (a, b) where
  toString (a, b) = "(" ++ toString a ++ "," ++ toString b ++ ")"



class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab x
        | doesEnrageGork x && doesEnrageMork x = stomp (stab x)
        | doesEnrageGork x = stab x
        | doesEnrageMork x = stomp x
        | otherwise = x


class (Enum a, Bounded a, Eq a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x
    | x == maxBound = minBound
    | otherwise = succ x

  spred :: a -> a
  spred x
    | x == minBound = maxBound
    | otherwise = pred x

instance SafeEnum Bool

class (Bounded a, Show a) => TestBnd a where
  getTop :: a
  getTop = maxBound

  getTopByExample :: a -> a
  getTopByExample a = maxBound

  getBot :: a
  getBot = minBound


instance TestBnd Bool
instance TestBnd Int

class (Num a) => TellType a where
    tell :: a -> String

instance TellType Integer where
    tell a = "Integer"

instance TellType Float where
    tell a = "Float"

instance TellType Double where
    tell a = "Double"

instance TellType Bool where
    tell a = "Bool"
--to get it working with type annotation need to call like "tell (4 :: Float)"
--coz "tell 4 :: Float" tries to get return type Float, not passing "4" as Float



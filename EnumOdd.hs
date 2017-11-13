data Odd = Odd Integer
  deriving (Eq, Show)

instance Enum Odd where
  succ (Odd x) = Odd(x+2)
  pred (Odd x) = Odd(x-2)
  toEnum x = Odd(toInteger x)
  fromEnum (Odd x) = fromInteger x
  enumFrom odd@(Odd x) = [odd, succ odd ..]
  enumFromThen (Odd a) (Odd b) = [Odd a, Odd (b - a) ..]
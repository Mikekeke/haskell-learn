{-# LANGUAGE TypeOperators #-}
module TypeComposition where
    infixr 9 |.|
    newtype (|.|) f g a = Cmps {getCmps ::  f (g a)} deriving (Eq, Show)

    type A   = ((,) Integer |.| (,) Char) Bool
    type B t = ((,,) Bool (t -> t) |.| Either String) Int
    type C   = (|.|) ((->) Bool) ((->) Integer) Integer

    a :: A
    a = Cmps $ (2,('a',True))

    b :: B t
    b = Cmps $ (True, id, Right 3)

    c :: C
    c = Cmps $ \p x -> if p then x else x+x
    --from solutions
    c1 = Cmps seq
    c2 = Cmps $ flip const
    c3 = Cmps $ const $ const 3


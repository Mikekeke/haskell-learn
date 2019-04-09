{-# LANGUAGE TypeOperators #-}
-- :set -XTypeOperators - to use in ghci
module TypeComposition where
    infixr 9 |.|
    newtype (|.|) f g a = Cmps {getCmps ::  f (g a)} deriving (Eq, Show)

    -- :t Cmps (Just "abc") :: (Maybe |.| []) Char
    --    ~> (Just "abc") :: (Maybe |.| []) Char :: (|.|) Maybe [] Char

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

    instance (Functor f, Functor g) => Functor (f |.| g) where
    --  fmap :: (a -> b) -> (f |.| g) a -> (f |.| g) b
    --  fmap :: (a -> b) -> (|.|) f g a -> (|.|) f g b
        fmap fn cmps = Cmps $ (fmap.fmap) fn (getCmps cmps)
    --  fmap fn (Cmps x) = Cmps $ fmap (fmap fn) x

    instance (Applicative f, Applicative g) => Applicative (f |.| g) where
        pure = Cmps . pure . pure
        (Cmps fn) <*> (Cmps x) = Cmps $ fmap (<*>) fn <*> x
--                                             1       2
{-
fn :: f (g (a -> b))
x :: f (g a)
result must be :: f (g b)

#1 (<*>) :: g (a -> b) -> g a -> g b
      ~> :: g (a -> b) -> (g a -> g b)
so, fmap (<*>) :: f (g (a -> b)) -> f (g a -> g b)

now we have:
fmap (<*>) :: f (g (a -> b)) -> f (g a -> g b)
fn :: f (g (a -> b))
x :: f (g a)

then, fmap (<*>) fn :: f (g a -> g b) - where "f" is applicative finctor
and we can use #2 (<*>) to apply "f (g a -> g b)" to "x :: f (g a)" and get result "f (g b)"
daayum...
-}


    -- λ: pure 42 :: ([] |.| [] |.| []) Int
    -- Cmps {getCmps = [Cmps {getCmps = [[42]]}]}
    -- λ: pure 42 :: ([] |.| [] |.| [] |.| []) Int
    -- Cmps {getCmps = [Cmps {getCmps = [Cmps {getCmps = [[42]]}]}]}
    -- a = Cmps {getCmps = [Cmps {getCmps = [[42]]}]}
    -- b = getCmps <$> a
    -- ~> b = [Cmps {getCmps = [[42]]}]
    -- c = getCmps <$> b
    -- ~> c = [[[42]]]

    unCmps3 :: Functor f => (f |.| g |.| h) a -> f (g (h a))
    unCmps3 x = getCmps <$> getCmps x
    unCmps4 :: (Functor f2, Functor f1) => (f2 |.| f1 |.| g |.| h) a -> f2 (f1 (g (h a)))
    unCmps4 x = unCmps3 <$> getCmps x
    -- unCmps3 = (getCmps <$>) . getCmps - ugly pointfree
    -- more variants
    -- unCmps3 x = fmap getCmps (getCmps x)
    -- unCmps3 = fmap getCmps . getCmps -- same poinfree
    -- unCmps4 = fmap unCmps3 . getCmps

    newtype Cmps3 f g h a = Cmps3 { getCmps3 :: f (g (h a)) } deriving (Eq,Show)

    instance (Functor f, Functor g, Functor h) => Functor (Cmps3 f g h) where
        fmap fn = Cmps3 . (fmap.fmap.fmap $ fn) . getCmps3
        -- fmap = (Cmps3 .) . (. getCmps3) . fmap . fmap . fmap


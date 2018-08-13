{-# LANGUAGE TypeOperators #-}
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

    newtype Cmps3 f g h a = Cmps3 { getCmps3 :: f (g (h a)) } deriving (Eq,Show) 
    
    instance (Functor f, Functor g, Functor h) => Functor (Cmps3 f g h) where
        fmap fn = Cmps3 . (fmap.fmap.fmap $ fn) . getCmps3
        -- fmap = (Cmps3 .) . (. getCmps3) . fmap . fmap . fmap


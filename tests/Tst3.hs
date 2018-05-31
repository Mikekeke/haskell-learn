find' :: (a -> Bool) -> [a] -> Maybe a
find' p = foldr (\x z -> if p(x) then Just x else z) Nothing

fn1 x | x > 1 = Right x
      | otherwise = Left "error fn1"

fn2 x | x > 3 = Right x
      | otherwise = Left "error fn2"

t1 x = fn1 x >>= fn2

newtype Vald a b = Vald {getEth :: Either a b}
instance Functor (Vald a) where
    fmap f vld = Vald $ fmap f (getEth vld)

instance Monoid a => Applicative (Vald a) where
    pure = Vald . Right
    (Vald (Right f)) <*> v = fmap f v
    (Vald (Left s1)) <*> (Vald (Right _)) = Vald $ Left s1
    (Vald (Left m1)) <*> (Vald (Left m2)) = Vald $ Left $ mappend m1 m2



-- instance Monoid a => Monad (Vald a) where
--     return = Vald . Right
--     (Vald (Right x)) >>= k = k x
--     (Vald (Left x)) >>= _ = Vald (Left x)

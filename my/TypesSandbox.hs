f :: a -> b -> c
f = undefined

f1 :: b -> c
f1 = f id

f2 :: b -> c
f2 = f const

liftA2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
liftA2 _ Nothing _ = Nothing
liftA2 _ _ Nothing = Nothing
liftA2 f' (Just x) (Just y) = Just (f' x y)
{-
so if e.g. x = (+10)
liftA2 id (Just (+10)) (Just y)
Just (id (+10) x)
Just (x + 10)

which equivalent to  (<*>)
-}
liftA2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
liftA2 _ Nothing _ = Nothing
liftA2 _ _ Nothing = Nothing
liftA2 f' (Just x) (Just y) = Just (f' x y)
{-
investigation started from http://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.Base.html#liftA2
(<*>) = liftA2 id

so if e.g. x = (+10)
liftA2 id (Just (+10)) (Just y)
Just (id (+10) x)
Just (x + 10)

which equivalent to  (<*>)
-}

{-
from SO, if-then-esle shorthand
https://stackoverflow.com/questions/211216/hidden-features-of-haskell

True ? x = const x
False ? _ = id
will define (?) to be the ternary operator:

(a ? b $ c)  ==  (if a then b else c)
-}
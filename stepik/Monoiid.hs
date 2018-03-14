module Monoiid where

newtype Xor = Xor { getXor :: Bool }
    deriving (Eq,Show)

instance Monoid Xor where
    mempty = Xor False
    mappend (Xor a) (Xor b)
            | a /= b = Xor True
            | otherwise = Xor False

-- sure there was a better one
-- instance Monoid Xor where
--     mempty = Xor False
--     Xor x `mappend` Xor y = Xor (x /= y)

-- just for curiosity from video; already has instance in base
-- instance (Monoid a, Monoid b) => Monoid (a,b) where
--     mempty = (mempty, mempty)
--     (x1,x2) `mappend` (y1,y2) = (x1 `mappend` y1, x2 `mappend` y2)


-- *********************************
newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

instance Monoid a => Monoid (Maybe' a) where
    mempty = Maybe' (Just mempty)

    -- my, see below why not working (another unexplained fucking-with-syntax exercise)
    -- Maybe' (Just mempty) `mappend` m = m
    -- m `mappend` Maybe' (Just mempty)  = m

    mappend (Maybe' Nothing) _ = Maybe' Nothing
    mappend _ (Maybe' Nothing) = Maybe' Nothing
    Maybe' x `mappend` Maybe' y = Maybe' (mappend x y)

--Интересно, почему строки

    -- mappend (Maybe' (Just mempty)) b = b
    -- mappend a (Maybe' (Just mempty)) = a

--делают решение неверным? Это ведь фактически определение пустого элемента.

--Потому что `mempty` это всего лишь идентификатор, вы там могли что угодно написать, хоть `mempty`, хоть `x`, хоть `foobar`.

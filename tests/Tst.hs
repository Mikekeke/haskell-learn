-- data Eth a b = Lft a | Rght b

-- instance Functor (Eth a) where
--     fmap f (Rght b) = Rght (f b)
--     fmap _ (Lft a)  = Lft a

-- instance Monoid a => Applicative (Eth a) where
--     pure = Rght
--     (Rght f) <*> (Rght a) = Rght $ f a
--     (Rght f) <*> (Lft e) = Lft e
--     (Lft e) <*> (Rght a) = Lft e
--     (Lft e) <*> (Lft e1) = Lft (e `mappend` e1)

-- instance Monoid a => Monad (Eth a) where
--     return = Rght
--     (Rght f) >>= k = k f
--     (Lft e) >>= k = case k e of
--         (Rght _) -> Lft e
--         (Lft e1) -> Lft (e `mappend` e1)
--     -- (Rght f)
--     -- (Lft e)
--     -- (Lft e)
import           Data.Monoid

newtype Meth a b = Meth {getMeth :: Either a b} deriving Show
mR = Meth . Right
mL = Meth . Left

newtype Log a = Log {getL :: [a]} deriving Show

instance Monoid (Log a )where
    mempty = Log []
    l1 `mappend` l2 = Log $ (getL l1) ++ (getL l2)

logShit a = Log [a]

instance Functor (Meth a) where
    fmap f (Meth (Right a)) = Meth $ Right (f a)
    fmap _(Meth (Left  e))  = Meth $ Left e

instance Monoid e => Applicative (Meth e) where
    pure = Meth . Right
    (Meth (Right f)) <*> (Meth (Right a)) = Meth $ Right (f a)     -- neutral
    (Meth (Left  e)) <*> (Meth (Right _)) = Meth $ Left e          -- short-circuit
    (Meth (Right _)) <*> (Meth (Left  e)) = Meth $ Left e          -- short-circuit
    (Meth (Left e1)) <*> (Meth (Left e2)) = Meth $ Left (e1 <> e2)

hasA s = if elem 'a' s then mR s else mL $ logShit "no a"
hasB s = if elem 'b' s then mR s else mL $ logShit "no b"
hasC s = if elem 'c' s then mR s else mL $ logShit "no c"

test s = getMeth $ (,,) <$> hasA s <*> hasB s <*> hasC s

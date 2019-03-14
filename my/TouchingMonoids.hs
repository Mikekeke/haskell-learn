class Monoid1 a where
    memptyest :: a
    mappendy :: a -> a -> a

instance Monoid1 b => Monoid1 (a -> b) where
    memptyest _ = memptyest
    mappendy f g x = f x `mappendy` g x
    -- mappendy f g = \x -> f x `mappendy` g x

-- λ: import Data.Monoid
-- λ: :t [(+2), (*10)]
-- [(+2), (*10)] :: Num a => [a -> a]
-- λ: :t mconcat [(+2), (*10)]
-- mconcat [(+2), (*10)] :: (Num a, Monoid a) => a -> a
-- λ: mconcat [(+2), (*10)] $ (Sum 3)
-- Sum {getSum = 35}
-- λ: mconcat [(+2), (*10), (*100)] $ (Sum 3)
-- Sum {getSum = 335}
-- λ: mconcat [(+2), (*10), (*100)] $ (Product 3)
-- Product {getProduct = 45000}
-- λ: mconcat [(+2)] $ (Product 3)
-- Product {getProduct = 5}
-- λ: mconcat [(+2), (+3)] $ (Product 3)
-- Product {getProduct = 30}
-- λ: (appEndo . mconcat . map Endo) [(+2), (*10)] 3
-- 32
-- λ: mappend (+3) (*2) (Sum 2)
-- Sum {getSum = 9}
-- λ: mappend (+3) (*2) (Product  2)
-- Product {getProduct = 20}
-- λ: mempty (Sum 3)
-- ()

-- λ: import Data.Foldable
-- λ: :t fold
-- fold :: (Monoid m, Foldable t) => t m -> m
-- λ: fs = Endo <$> [(+2), (*3)]
-- λ: :t fold fs
-- fold fs :: Num a => Endo a
-- λ: appEndo (fold fs) 3

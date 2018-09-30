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
import Control.Applicative

filterM ::(Applicative m) => (a -> m Bool) -> [a] -> m [a]
filterM p = foldr f (pure []) where
    -- cant just make separate function from lambda, coz need "a" from outside
    f a ma = (\bool -> if bool then (a:) else id) <$> (p a) <*> ma 

filterM' ::(Applicative m) => (a -> m Bool) -> [a] -> m [a]
filterM' p = foldr (\x z -> liftA2 (\bool -> if bool then (x:) else id) (p x) z) (pure [])  
--                   ^                                     ^
--                  need lift lambda coz no other way to access x

-- bit shorter
filterM'' ::(Applicative m) => (a -> m Bool) -> [a] -> m [a]
filterM'' p = foldr (\x -> liftA2 (\bool -> if bool then (x:) else id) (p x)) (pure [])

prd x = if x < 3 then Just True else Just False

testFilters :: (Applicative m) => [(a -> m Bool) -> [a] -> m [a]]
testFilters = [filterM, filterM', filterM'']
-- sequenceA testFilters :: Applicative m => (a -> m Bool) -> [[a] -> m [a]]
testFiltersWithPred :: (Ord a, Num a) => [a] -> [Maybe [a]]
testFiltersWithPred = sequenceA $ sequenceA testFilters prd

test = testFiltersWithPred [1,2,3,4]

test' :: [Maybe[Integer]]
test' = (sequenceA . sequenceA testFilters $ prd) [1,2,3,4]
{-
!to-understand
sequenceA == traverse id
traverse id [(\x -> x + 2)]
foldr (\a b -> (:) <$> id a <*> b) (pure []) [(\x -> x + 2)]

(\a b -> (:) <$> id a <*> b) (\x -> x + 2) $ foldr (\a b -> (:) <$> id a <*> b) (pure []) []
(\a b -> (:) <$> id a <*> b) (\x -> x + 2) (pure [])
(:) <$> id (\x -> x + 2) <*> (pure [])
(:) <$> (\x -> x + 2) <*> (pure [])
(:) . (\x -> x + 2) <*> (pure [])
(:) . (\x -> x + 2) <*> (\_-> [])
((:) . (\x -> x + 2) <*> (\_-> [])) :: Num a => a -> [a]

λ: ((:) . (\x -> x + 2) <*> (\_-> [])) 2
[4]

???
((:) . (\x -> x + 2) <*> (\_-> []))
(\x -> (:)(x + 2)) <*> (\_-> [])) 
-- ff <*> f :: (e -> a -> b) -> (e -> a) -> (e -> b)
-- \e -> ff e $ f e
\e -> (\x -> (:)(x + 2)) e $ (\_-> []) e
\e -> (\x -> (:)(x + 2)) e $ []
\e -> (:)(e + 2) $ []
\e -> (e + 2) : []

-}

fn = \e -> (e + 2) : []
-- pointfree "fn e = (e + 2) : []"
-- fn = return . (2 +)
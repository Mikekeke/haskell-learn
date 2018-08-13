import Data.Monoid

group' :: Eq a => [a] -> [[a]]
group' [] = []
group' [x] = [[x]]
group' (x:xs) | x == head xs = let (x':xs') = group' xs in (x : x') : xs'
              | otherwise = [x] : group' xs
--   1:2:[]
--   [1,2]
--   λ: 1:2:[3]
--   [1,2,3]

data Validate e a = Invalid {errors :: e} | Valid a deriving Show
instance Functor (Validate e) where
    fmap f (Valid v) = Valid $ f v
    fmap _ (Invalid es) = Invalid es

instance Monoid e => Applicative (Validate e) where
    pure = Valid
    (Valid f) <*> (Valid v) = Valid $ f v
    (Invalid e1) <*> (Invalid e2) = Invalid $ e1 <> e2
    (Invalid e) <*> (Valid _) = Invalid e
    (Valid _) <*> (Invalid e) = Invalid e

data Obj = Obj Int String deriving Show
prsInt x = if x >= 10 then Valid x else Invalid ["error: x < 10"]
prsStr s = if (length s) < 5 then Invalid ["str too short"] else Valid s
parseObj x s = Obj <$> prsInt x <*> prsStr s
-- tstGood = (. prsStr) . (<*>) . (Obj <$>) . prsInt
tstGood = parseObj 12 "sobaka"
tstBad = parseObj 3 "kek"
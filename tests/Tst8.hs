import           Data.Monoid

import Debug.Trace

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
    fmap f (Valid v)    = Valid $ f v
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

sliding :: (a -> a -> Bool) -> Int -> [a] -> [[a]]
sliding p 0 _ = []
sliding p _ [] = []
sliding p n ls | length ls <= n = [ls]
             | otherwise = go n ls where
                go _ [] = []
                go n' (x:xs) | n' == 0 = [] :  sliding p n (tail ls)
                             | otherwise = let (y:ys) = go (n' - 1) xs in
                                if p x (head y) then (x : y) : ys else []

-- http://hackage.haskell.org/package/base-4.11.1.0/docs/src/Data.Foldable.html#foldlM
foldlM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldlM f z0 xs = foldr f' return xs z0
  where f' x k z = f z x >>=  k

isBalanced' xs = foldlM op [] xs
  where
    op ('(':xs) ')'          = trace "j" Just xs
    op ('[':xs) ']'          = trace "j" Just xs
    op ('{':xs) '}'          = trace "j" Just xs
    op xs x | x `elem` ")]}" = trace "n" Nothing
            | otherwise      = trace "j" Just (x:xs)


bnd :: Maybe a -> (a -> Maybe b) -> Maybe b
bnd (Just v) k = k v
bnd Nothing k = trace "n" Nothing
tst = trace "1" Just 4 `bnd` trace "2" const Nothing  `bnd` trace "3" const Nothing  `bnd` trace "4" const Nothing
-- > tst
-- 1
-- 2
-- n
-- n

foldlMm :: Foldable t => (b -> a -> Maybe b) -> b -> t a -> Maybe b
foldlMm f z0 xs = foldr f' return xs z0
  where f' x k z = f z x `bnd` k

-- а вот здесь 'n' не печатается для `bnd` после 1го Nothing
tst1 = foldlMm fn 0 [1..10000000] where fn z0 x = if x < 10 then Just (z0+x) else Nothing
tst2 = foldlMm fn 0 [1..10000000] where fn z0 x = if x < 1000000 then Just (z0+x) else Nothing
import Data.Fix hiding (cata)

data ListF a f = NilF | ConsF a f deriving (Show)

instance Functor (ListF a) where
    fmap _ NilF = NilF
    fmap f (ConsF a fk) = ConsF a (f fk)

type List a = Fix (ListF a)
testList :: List Int
testList  = Fix $ ConsF 1 (Fix $ ConsF 2 (Fix NilF))

data MaybeF a c = NothingF | JustF a deriving (Show)
instance Functor (MaybeF a) where
  fmap _  NothingF = NothingF
  fmap _ (JustF x) = JustF x

type MyMaybe a = Fix (MaybeF a)

-- newtype Fix f = Fix { unFix :: f (Fix f) }
-- cata :: Functor f => (f a -> a) -> (Fix f -> a) -- from hackage
cata :: Functor f => (f a -> a) -> Fix f -> a
cata f fixed = f  (fmap (cata f) (unFix fixed))

maybeAlg unfixed = case unfixed of 
        NothingF -> Nothing
        JustF x -> Just x

listAlgebra unfixed = case unfixed of 
        NilF -> []
        ConsF a rest -> a : rest
{-
λ: cata listAlgebra testList
[1,2]
-}


t1 = cata listAlgebra (Fix $ ConsF 2 (Fix NilF))
t2 = listAlgebra (fmap (cata listAlgebra) (ConsF 2 (Fix NilF)))
t3 = listAlgebra (ConsF 2 (cata listAlgebra (Fix NilF)))
t4 = listAlgebra (ConsF 2 (listAlgebra (fmap (cata listAlgebra) NilF)))
t5 = listAlgebra (ConsF 2 (listAlgebra NilF))
t6 = listAlgebra (ConsF 2 [])
t7 =  2 : []

----------------------------------------------------------------------------------------------------
data Tree a = Tree {elem :: a, leaves :: [Tree a]}

instance Functor Tree where
    fmap f (Tree a lvs) = Tree (f a) ((fmap f) <$> lvs)

instance Foldable Tree where
    foldr f z (Tree a []) = f a z
    foldr f z (Tree a lvs) = f a (foldr fn z lvs) where
        fn t b = foldr f b t

-- https://stackoverflow.com/questions/22003307/depth-of-a-tree-haskell
generalFold :: (a -> [t] -> t) -> Tree a -> t
generalFold fn t = h t where
    h (Tree a lvs) = fn a (map h lvs)
{-
maxOr0 [] = 0
maxOr0 xs = maximum xs

height :: Tree a -> Int
height = generalFold maxPlus1 where
      maxPlus1 a as = 1 + maxOr0 as

sumTree = generalFold sumNode where
      sumNode a as = a + sum as

productTree = generalFold productNode where
      productNode a as = a * product as

longestPath = generalFold longest where
      longest a as = a + maxOr0 as
-}

foldrViaGen :: (a -> b -> b) -> b -> Tree a -> b
foldrViaGen f z t = generalFold (\a as -> f a (foldr undefined z as)) t 

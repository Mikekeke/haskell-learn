import Data.Fix hiding (cata)

data ListF a f = NilF | ConsF a f deriving (Show)

instance Functor (ListF a) where
    fmap _ NilF = NilF
    fmap f (ConsF a fk) = ConsF a (f fk)

type List a = Fix (ListF a)
testList :: List Int
testList  = Fix $ ConsF 1 (Fix $ ConsF 2 (Fix NilF))

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

{-# LANGUAGE  DeriveFunctor #-}
import Data.Fix

-- Fix f = f (Fix f)
data ListF a r = NilF | ConsF a r deriving (Show, Eq, Functor)
type List a = Fix (ListF a)

nil = Fix NilF
cons :: a -> List a -> List a
cons a l = Fix (ConsF a l)

recGo :: Functor f => (Fix f -> Fix f) -> Fix f -> Fix f
recGo f l = f (Fix (fmap f (unFix l)))

tstLF = cons 1 (cons 2 nil)
-- Fix (ConsF 1 (Fix (ConsF 2 (Fix NilF))))

-- toList :: List a -> [a]
-- toList lf = recGo (extract) lf
--     where extract (ConsF v _) = v
--           extract NilF = []
          

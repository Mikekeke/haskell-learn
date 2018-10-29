import Test.QuickCheck


class Semigroup a where
    (<>) :: a -> a -> a

newtype Sum = Sum {getSum :: Int} deriving Show

instance Semigroup Sum where
    a <> b = Sum $ getSum a + getSum b
    
data Trivial = Trivial deriving (Eq, Show)
instance Semigroup Trivial where
   _ <> _ = Trivial
instance Arbitrary Trivial where
    arbitrary = return Trivial

newtype BoolConj = BoolConj Bool deriving (Eq, Show)
instance Semigroup BoolConj where
    _                <> (BoolConj False) = BoolConj False
    (BoolConj False) <> _ = BoolConj False
    _                <> _ = BoolConj True
instance Arbitrary BoolConj where
    arbitrary = BoolConj <$> choose (False, True)


newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)
instance Semigroup BoolDisj where
    _               <> (BoolDisj True) = BoolDisj True
    (BoolDisj True) <> _ = BoolDisj True
    _               <> _ = BoolDisj False

instance Arbitrary BoolDisj where
    arbitrary = BoolDisj <$> choose (False, True)

newtype Combine a b = Combine { unCombine :: (a -> b) }
instance (Semigroup b) => Semigroup (Combine a b) where
    (Combine f1) <> (Combine f2) = Combine $ (<>) <$> f1 <*> f2
{-
λ: unCombine (f <> g) $ 0
Sum {getSum = 0}
λ: unCombine (f <> g) $ 1
Sum {getSum = 2}
-}

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool
type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

main :: IO ()
main = 
    quickCheck (semigroupAssoc :: TrivialAssoc) 
    >> quickCheck (semigroupAssoc :: BoolConjAssoc)
    >> quickCheck (semigroupAssoc :: BoolDisjAssoc)
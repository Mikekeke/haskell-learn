module SomeDataTests where



data MyNum1 = MyNum1 Int deriving (Show, Ord, Eq)

data MyNum2 = MyNum2 Int
instance Show MyNum2 where
    show (MyNum2 _) = "lol"

data MyNum3 = MyNum3 Int
instance Show MyNum3 where
    show (MyNum3 x) = "This is [MyNum3 " ++ show x ++"]"

--
class Bananable a where
    toBanana :: a -> String
    toBanana _ = "empty banana, sad"

instance Bananable MyNum1 where
    toBanana (MyNum1 x) = "Banana with " ++ show x

instance Bananable MyNum3 where
    toBanana mn@(MyNum3 666) = "Satan Banana with -" ++ show mn ++ "-!"
    toBanana mn = "Banana with -" ++ show mn ++ "-"

instance Bananable()

data SmData a = SmData a a deriving Show
class Sumble a where
    sumbl :: a -> String

instance (Show a, Num a) => Sumble (SmData a) where
     sumbl (SmData x1 x2) = "Int sum = " ++ show (x1+x2)
--
-- instance ([] t) => Sumble (SmData [t]) where
--      sumbl _ = "ddd"
--      sumblL _ = "ddd"
--      sumbl (SmData x1 x2) = "Int sum = " ++ show (x1+x2)

-- instance (Functor a) => Sumble (SmData a) where
--      sumbl (SmData x1 x2) = "Int sum = " ++ show (x1+x2)


-- sumbl2 (SmData x1 x2) = "Int sum = " ++ show (x1+x2)
-- sumbl2 (SmData l1@(x:xs) l2@(y:ys)) = "Int sum = " ++ show (l1++l2)

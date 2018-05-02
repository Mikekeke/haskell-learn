import           Control.Applicative

f >$< x = getZipList . fmap f $ ZipList x
af >*< aa = getZipList $ ZipList af <*> ZipList aa

x1s = [1,2,3]
x2s = [4,5,6]
x3s = [7,8,9]
x4s = [10,11,12]



newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }

instance Functor (Arr2 e1 e2) where
  fmap f = Arr2 . (fmap . fmap $ f) . getArr2

instance Functor (Arr3 e1 e2 e3) where
  fmap f = Arr3 . (fmap . fmap . fmap $ f) . getArr3

instance Applicative (Arr2 e1 e2) where
  pure x = Arr2 $ \e1 e2 -> x
  (Arr2 f) <*> (Arr2 v) = Arr2 $ \e1 e2 -> f e1 e2 $ v e1 e2
--   Arr2 f <*> Arr2 g = Arr2 $ \e1 e2 -> (f e1 e2) . (g e1) $ e2 -- not my, keep in mind


instance Applicative (Arr3 e1 e2 e3) where
    pure x = Arr3 $ \e1 e2 e3 -> x
    (Arr3 f) <*> (Arr3 v) = Arr3 $ \e1 e2 e3 -> (f e1 e2 e3) $ (v e1 e2 e3)

-- stuff's answer
-- instance Functor (Arr2 e1 e2) where
--     fmap = liftA

--   instance Functor (Arr3 e1 e2 e3) where
--     fmap = liftA

--   instance Applicative (Arr2 e1 e2) where
--     pure x = Arr2 (\e1 e2 -> x)
--     (<*>) (Arr2 g) (Arr2 h) = Arr2 $ liftA2 (<*>) g h

--   instance Applicative (Arr3 e1 e2 e3) where
--     pure x = Arr3 (\e1 e2 e3 -> x)
--     (<*>) (Arr3 g) (Arr3 h) = Arr3 $ (liftA2 . liftA2) (<*>) g h

-- keep in mind
-- (*>) :: Applicative f => f a -> f b -> f b
-- [1,2,3] *> [4,5] == [4,5,4,5,4,5]

-- v <* v = pure const <*> u <*> v
-- [1,2,3] <* [4,5] == [1,1,2,2,3,3]
-- Just 4 *> Just 5 == Just 5
-- Nothing *> Just 5 == Nothing
-- result wil have structure of both contaners with elements from second one

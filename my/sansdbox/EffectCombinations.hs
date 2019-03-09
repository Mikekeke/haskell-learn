import Control.Applicative

t1_1 = fmap succ (Right 1) :: Either () Int
t1_2 = (fmap . fmap $ succ) $ Just (Right 1) :: Maybe (Either () Int)
t1_2' = fmap succ <$> Just (Right 1) :: Maybe (Either () Int)

t2_1 = Right succ <*> Right 1
t2_2 = liftA2 (<*>) (Just (Right succ)) (Just (Right 1))
t2_3 = liftA2 (liftA2 (<*>)) (Right (Just (Right succ))) (Right (Just (Right 1)))

t3 = undefined -- should be monads, need transformers

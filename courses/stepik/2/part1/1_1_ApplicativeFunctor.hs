import           Control.Applicative

newtype Arr2 e1 e2 a = Arr2 {getArr2 :: e1 -> e2 -> a}
newtype Arr3 e1 e2 e3 a = Arr3 {getArr3 :: e1 -> e2 -> e3 -> a}

instance Functor (Arr2 e1 e2) where
  fmap f (Arr2 fun) = Arr2 $ \e1 e2 -> f $ fun e1 e2
--   fmap f (Arr2 g)= Arr2 $ fmap f . g -- not my

instance Functor (Arr3 e1 e2 e3) where
  fmap f (Arr3 fun) = Arr3 $ \e1 e2 e3 -> f $ fun e1 e2 e3
--   fmap f (Arr3 g) = Arr3 (\x y -> fmap f $ g x y) -- not my

test1 = getArr2 (fmap length (Arr2 take)) 10 "abc"
test2 = getArr3 (tail <$> tail <$> Arr3 zipWith) (+) [1,2,3,4] [10,20,30,40,50]

{-
    !!! stuff's variant and commentary
    instance Functor (Arr2 e1 e2) where
        fmap f = Arr2 . (fmap . fmap $ f) . getArr2

      instance Functor (Arr3 e1 e2 e3) where
        fmap f = Arr3 . (fmap . fmap . fmap $ f) . getArr3

    Представьте, что у вас есть значение в контейнере внутри ещё одного контейнера — например,`Maybe [a]`.
    И `Maybe` является функтором, и список является функтором.
    Как мне теперь применить функцию `f :: a -> b` к лежащим там значениям?
    Ну, если бы у меня был только список, то я бы сделал на него `fmap f :: [a] -> [b]` и готово.
    Заменим, что теперь у меня есть моё исходное `Maybe [a]` и новая функция `[a] -> [b]`, соответственно,
    я теперь могу эту функцию через fmap применить к исходному контейнеру,
    то есть `fmap (fmap f) :: Maybe [a] -> Maybe [b]`.
    Если записать это через композицию, получится `fmap . fmap $ f`.
    В этой задаче ситуация как раз такая, надо только помнить, что функции тоже являются инстансами функтора,
    т.е. я в своем решении смотрю на функцию `x -> y -> a` как на значение типа `a`, лежащее внутри функтора `(->) y`,
    и всё это внутри функтора `(->) x`.

    λ: :t fmap . fmap $ take
    fmap . fmap $ take:: (Functor f2, Functor f1) => f1 (f2 Int) -> f1 (f2 ([a] -> [a]))
-}


tst1 = Just (+) <*> Just 5 <*> Just 6
tst2 = liftA2 (+) (Just 5) (Just 6)
tst3 = (+) <$> Just 5 <*> Just 6
res = maybe False (all (==11)) . sequence $  [tst1, tst2, tst3]
resMaybe = pure (all (==11)) <*> sequence [tst1, tst2, tst3]


tst21 = Just (+2) <*> pure 4
-- same
tst22 = pure ($ 4) <*> Just (+2)
-- explanation:
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- ($ 4) :: Num a => (a -> b) -> b
-- fn = ($ 4)
-- fn (+10) will be 14


tst4 = pure (.) <*> Just (+100) <*> Just (^2) <*> Just 4 -- == Just 116
tst4' = (.) (+100) (^2) 4   -- == 116
tst4'' = (+100) . (^2) $ 4  -- == 116


data Triple a = Tr a a a  deriving (Eq,Show)

instance Functor Triple where
  fmap f (Tr x y z) = Tr (f x) (f y) (f z)

instance Applicative Triple where
  pure x = Tr x x x
  (Tr f1 f2 f3) <*> (Tr x y z) = Tr (f1 x) (f2 y) (f3 z)

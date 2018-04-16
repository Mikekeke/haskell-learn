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

import           Control.Applicative
-- >>> filtering (>) (4 :. 5 :. 6 :. 7 :. 8 :. 9 :. 10 :. 11 :. 12 :. Nil) 8
-- [9,10,11,12]

filtering ::
  Applicative f =>
  (a -> f Bool)
  -> [a]
  -> f [a]

filtering f l = \x -> filter <$> f l
-- filtering _ []        = pure []
-- filtering fa (x : xs) = liftA2 (:) (res *> fx) (filtering fa xs) where
--     fx = pure x
--     res = (fa x)
--     -- comp = res == (res *> (pure True))

tp = (\a -> if a > 3 then Nothing else Just (a > 1))

foldFiltr p = foldl (\acc x -> if p x then x : acc else acc) []

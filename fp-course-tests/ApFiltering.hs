-- | Filter a list with a predicate that produces an effect.
--
-- >>> filtering (ExactlyOne . even) (4 :. 5 :. 6 :. Nil)
-- ExactlyOne [4,6]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. Nil)
-- Full [4,5,6]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. 7 :. 8 :. 9 :. Nil)
-- Full [4,5,6,7]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. 13 :. 14 :. Nil)
-- Empty
--
-- >>> filtering (>) (4 :. 5 :. 6 :. 7 :. 8 :. 9 :. 10 :. 11 :. 12 :. Nil) 8
-- [9,10,11,12]
--
-- >>> filtering (const $ True :. True :.  Nil) (1 :. 2 :. 3 :. Nil)
-- [[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3]]
--

-- filtering ::
--   Applicative f =>
--   (a -> f Bool)
--   -> List a
--   -> f (List a)
-- filtering =
--   error "todo: Course.Applicative#filtering"
import Control.Applicative

filtering :: Applicative f => (a -> f Bool) -> [a] -> f [a]
filtering k l = undefined where
    efList :: Applicative f => (a -> f Bool) -> [a] -> f [a]
    -- efList f = foldr (\x z -> (:) <$> ((f x) *> pure x) <*> z) (pure [])
    efList f = foldr (\x -> liftA2 (:) (f x *> pure x)) (pure [])

tOpt x = if x > 3 then Just (x > 5) else Nothing
filteringMb = filtering tOpt


-- > let fn =  filter <$> (flip (>))
-- > fn 3 [1,2,3,4]
-- [4]
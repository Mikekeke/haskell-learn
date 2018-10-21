import Control.Applicative
traverse2list :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f [b]
-- traverse2list f = foldr (\x b -> (:) <$> f x <*> b) (pure [])
traverse2list f = foldr (liftA2 (:) . f) (pure [])

-- from solutions
-- import           Data.Foldable
-- traverse2list :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f [b]
-- traverse2list = (. toList) . traverse
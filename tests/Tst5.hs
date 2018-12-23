import Data.Monoid


fldr :: (a -> b -> b) -> b -> [a] -> b
fldr f z c = appEndo (foldMap (Endo . f) c) z
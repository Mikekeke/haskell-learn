import Data.Monoid


fldr :: (a -> b -> b) -> b -> [a] -> b
fldr f z c = appEndo (foldMap (Endo . f) c) z

fldl :: (b -> a -> b) -> b -> [a] -> b
fldl _ z [] = z
fldl f z (x:xs) = fldl f (f z x) xs
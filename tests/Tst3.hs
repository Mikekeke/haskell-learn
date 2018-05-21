find' :: (a -> Bool) -> [a] -> Maybe a
find' p = foldr (\x z -> if p(x) then Just x else z) Nothing



-- https://twitter.com/Anka213/status/1176720292837830661?s=19

-- build f = f (:) []
-- map f xs = build (\c n -> foldr (c . f) n xs)`, 
-- `map f (map g xs) 
--     = build (\c n -> foldr (c.f) n (build (\c' n'-> foldr (c’.g) n' xs))) 
--     = build (\c n -> foldr (c.f.g) n xs) = map (f.g) xs
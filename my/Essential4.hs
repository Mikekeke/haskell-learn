null' :: [a] -> Bool
null' = foldr (\_ _ -> False) True
--              ^^^ оба значения попадают в функцию и игнорируются
{-
null [1,2]
~> foldr f True [1,2]
~> f 1 (foldr f True [2])
-- оба значения f игнорируются, возвращается False
~> False
-}
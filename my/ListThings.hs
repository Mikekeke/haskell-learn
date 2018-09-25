removeSublist :: Eq a => [a] -> [a] -> [a]
removeSublist _ [] = []
removeSublist [] s = s
removeSublist sub s@(c:cs) | null sub = s
                             | null s = []
                             | sub == l = r
                             | null r = s
                             | otherwise = c : removeSublist sub cs
                              where
                                subLen = length sub
                                (l,r) = splitAt subLen s
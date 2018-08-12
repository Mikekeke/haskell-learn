data Pers = Pers {name :: String} deriving Show
pList = Pers <$> ["Aaa", "B", "Cc"]
srtBy :: Ord b => (a -> b) -> [a] -> [a]
srtBy _ []     = []
srtBy f (x:xs) = filtFun (>) ++ [x] ++ filtFun (<=) where
    filtFun sign = srtBy f (filter ((sign $ f x) . f) xs)
testSrt = srtBy (length.name) pList --[Pers {name = "B"},Pers {name = "Cc"},Pers {name = "Aaa"}]

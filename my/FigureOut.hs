module FigureOut where

perms :: (Eq a) => [a] -> [[a]]
perms []= [[]]
perms (x:[])= [[x]]
perms l = concatMap (\x -> map (\xs -> x:xs) (perms(filter (/=x) l))) l


perms' :: [a] -> [[a]]
perms' [] = [[]]
perms' [x] = [[x]]
perms' (x:xs) = concatMap (insertElem x) (perms' xs) where
     		insertElem x [] = [[x]]
			insertElem x yss@(y:ys) = (x:yss) : map (y:)(insertElem x ys)
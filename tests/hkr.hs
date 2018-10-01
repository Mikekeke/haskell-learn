import Data.List
import Data.List.Split

s = mconcat $ repeat "AB"
-- this one bit faster
rotate1 s = let
    (l,r,_) = foldr (\c (l1,r2,p) -> if p then (c:l1,r2, not p) else (l1,c:r2, not p)) ([],[],True) s 
    in r ++ l

rotate2 s = let
    (l,r) = foldr f ([],[]) $ chunksOf 2 s
    f tpl (l1,r2) = case tpl of
        (x:y:_) ->  (x:l1, y:r2)
        (x:[]) ->  (x:l1, r2)
    in r ++ l
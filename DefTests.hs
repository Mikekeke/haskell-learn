
-- {-# LANGUAGE FlexibleContexts #-}

type Entry = ([Char], ([Char], [Integer]))
-- ex :: [Entry]
ex = [("a", [("a11", [1,2]), ("a12", [3,4])])]

f :: Monoid b => [(a,b)] -> b
f = mconcat . map snd

-- append :: Entry -> [Entry] -> [Entry]
-- append en ens = 


fix f = let x = f x in x
fn = \rec' x -> if (x < 0) then rec' (x + 3) else x
test1 = fix fn (-10)

-- decode c [x] = x
-- decode c (x:xs) = foldl (\b x -> if b < x then b*x else x+b) x xs

-- square x c = c (x^2)
-- add a b c = c (a+b)

decode c = c []
as [x] _ _ = x
as l _ _ = let (x:xs) = reverse l in foldl (\b y -> if b < y then b*y else y+b) x xs
a = undefined
number = undefined

cpsfy n b c = c (n:b)

one = cpsfy 1
two = cpsfy 2
three = cpsfy 3
seventeen = cpsfy 17
twenty = cpsfy 20
hundred = cpsfy 100
thousand = cpsfy 1000

-- ff = decode twenty as a number
ff = decode one hundred twenty three as a number
{-

-}
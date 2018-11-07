type Ingredient = String
type Amount     = Int
type Recipe     = [(Ingredient, Amount)]
type Storage    = [(Ingredient, Amount)]

-- it stops early if not found unlike best solution, but anyway need to iterate whole receipe to find length
cakes :: Recipe -> Storage -> Int
cakes recipe storage = if length l /= length recipe then 0 else minimum l where
    l = do 
        (ingrRec, amtRec) <- recipe
        amtStor <- maybe [] pure (lookup ingrRec storage)
        return $ div amtStor amtRec

-- best solution
-- cakes :: Recipe -> Storage -> Int
-- cakes recipe storage = minimum $ map (\ (w,q) -> maybe 0 (`div` q) $ lookup w storage) recipe

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs else []

cakes' :: Recipe -> Storage -> Int
cakes' recipe storage = minimum . takeWhileInclusive (/= 0 ) $ map (\ (w,q) -> maybe 0 (`div` q) $ lookup w storage) recipe

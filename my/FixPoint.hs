fix :: (t -> t) -> t
fix f = let x = f x in x

mFoldr :: (a -> b -> b) -> b -> [a] -> b
mFoldr _ z [] = z
mFoldr f z (x:xs) = f x (mFoldr f z xs)

nonreqFoldr :: ((a -> b -> b) -> b -> [a] -> b) -> (a -> b -> b) -> b -> [a] -> b
nonreqFoldr _ _ z [] = z
nonreqFoldr foldF f z (x:xs) = f x (foldF f z xs)
fixFoldr f z l = fixed f z l where
    fixed :: (a -> b -> b) -> b -> [a] -> b
    fixed = fix nonreqFoldr

ls = [1..4]
test1 :: (Num a, Eq a) => [a] -> Bool
test1 = (==) <$> (mFoldr (+) 0) <*> (fixFoldr (+) 0)

mFilter _ [] = []
mFilter p (x:xs) = if p x then x : (mFilter p xs) else mFilter p xs

fixFilter :: (t -> Bool) -> [t] -> [t]
fixFilter = fix (\rec p l -> case l of {
        [] -> []; 
        (x:xs) ->  if p x then x : (rec p xs) else rec p xs
        }
    )

fn x = if x < 3 then 3 else fn (x - 1)
fn' = fix $ \f x ->  if x < 3 then 3 else f (x - 1)
-- fn 30 == fn' 20 is True
    
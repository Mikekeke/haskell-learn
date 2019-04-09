import Debug.Trace
import Control.Applicative (Alternative (..), (<|>))


newtype PrsEP a = PrsEP { runPrsEP :: Int -> String -> (Int, Either String (a, String)) }

parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p  = snd . runPrsEP p 0

-- my initial
-- satisfyEP p = PrsEP f where
--     f i s | null s = (next, Left $ "pos " ++ show next ++ ": unexpected end of input")
--           | p c = (next, Right (c, cs))
--           | otherwise = (next, Left $ "pos " ++ show next ++ ": unexpected " ++ [c])
--            where 
--             next = succ i
--             (c:cs) = s

-- better (?) after reading solutions
satisfyEP p = PrsEP (f . succ) where
    f i s | null s = (i, Left $ "pos " ++ show i ++ ": unexpected end of input")
          | p c = (i, Right (c, cs))
          | otherwise = (i, Left $ "pos " ++ show i ++ ": unexpected " ++ [c])
           where (c:cs) = s

charEP :: Char -> PrsEP Char
charEP c = satisfyEP (== c)

{-
best from solutions
satisfyEP :: (Char -> Bool) -> PrsEP Char
satisfyEP pr = PrsEP (f . succ) where
    f pos []                 = (pos, Left ("pos " ++ show pos ++ ": unexpected end of input"))
    f pos (c:cs) | pr c      = (pos, Right (c, cs))
                 | otherwise = (pos, Left ("pos " ++ show pos ++ ": unexpected " ++ [c]))
-}

instance Functor PrsEP where
    fmap f (PrsEP g) = PrsEP $ \i s -> fmap (fmap $ \(v,s1) -> (f v, s1)) (g i s)

instance Applicative PrsEP where
    pure x = PrsEP $ \i s -> (i, Right (x, s))
    (PrsEP pAp) <*> (PrsEP p) = PrsEP $ \i s -> 
        case pAp i s of
            (i1, Left e) -> (i1, Left e)
            (i1, Right (f, s1)) -> case p i1 s1 of
                (i2, Left e) -> (i2, Left e)
                (i2, Right (v, s2)) -> (i2, Right (f v, s2))

anyEP = satisfyEP (const True)
testP = (,) <$> anyEP <* charEP 'B' <*> anyEP

{-
staff's solution
instance Functor PrsEP where
  fmap f = PrsEP . (fmap . fmap . fmap . fmap $ \(a, s) -> (f a, s)) . runPrsEP

instance Applicative PrsEP where
  pure x = PrsEP $ \i s -> (i, Right (x, s))
  pf <*> px = PrsEP $ \i s -> case runPrsEP pf i s of
                (i', Left e) -> (i', Left e)
                (i', Right (f, s')) -> runPrsEP (f <$> px) i' s'
-}

instance Alternative PrsEP where
    empty = PrsEP $ \i _ -> (i, Left $ "pos " ++ show i ++ ": empty alternative")
    pl<|>pr = PrsEP $ \i s -> case runPrsEP pl i s of
        lftP@(i1, Left _) -> case runPrsEP pr i s of
            rghtP@(i2, Left _) -> if i1 >= i2 then lftP else rghtP
            right -> right
        right -> right

tripleP [a,b,c] = (\x y z -> [x,y,z]) <$> charEP a <*> charEP b <*>  charEP c

newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

instance Functor PrsE where
    fmap f = PrsE . (fmap . fmap $ \(a, s) -> (f a, s)) . runPrsE
    
instance Applicative PrsE where
    pure x = PrsE $ \s -> Right (x, s)
    pf <*> px = PrsE $ \s -> case runPrsE pf s of
                                Left e        -> Left e
                                Right (f, s') -> runPrsE (f <$> px) s'

instance Monad PrsE where
    m >>= k = PrsE $ \s -> case runPrsE m s of
            Left e        -> Left e
            Right (v, s') -> runPrsE (k v) s'

{- from answers

instance Monad PrsE where
  return x = PrsE $ \s-> Right (x, s)
  (PrsE pa) >>= f = PrsE $ \s -> do
    (x , s') <- pa s
    runPrsE (f x) s'

instance Monad PrsE where
  (>>=) (PrsE v) k = PrsE $ \s -> (v s) >>= uncurry (runPrsE . k)

-}

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un x) (Un y) odd' = Bi x y odd'
concat3OC (Un x1) (Bi y1 y2 oddC) c = Bi x1 y1 (concat3OC (Un y2) oddC c)
concat3OC (Bi x1 x2 u) b c = Bi x1 x2 (concat3OC u b c)

tst1 = Bi 'a' 'b' (Un 'c')
tst2 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
tst3 = Bi 'i' 'j' (Un 'k')
referenceBi = Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))
testRes1 = concat3OC tst1 tst2 tst3 == referenceBi

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un oddC) = oddC
concatOC (Bi oddC1 oddC2 oddC) = concat3OC oddC1 oddC2 (concatOC oddC)

testRes2 = (concatOC $ Bi tst1 tst2 (Un tst3)) == referenceBi

instance Functor OddC  where
    fmap f (Un a) = Un $ f a
    fmap f (Bi a b un) =  Bi (f a) (f b) (fmap f un)

instance Applicative OddC where
    pure = Un
    (Un f) <*> (Un a) = Un $ f a
    (Un f) <*> bi = fmap f bi 
    (Bi f g oddAp) <*> (Un a) = Bi (f a) (g a) $ oddAp <*> (Un a)
    (Bi f g oddAp) <*> bi = concatOC $ Bi (f <$> bi) (g <$> bi) (Un $ oddAp <*> bi)

instance Monad OddC where
    return = Un
    (Un a) >>= k = k a
    (Bi a b oddC) >>= k = concatOC $ Bi (k a) (k b) $ Un (oddC >>= k) 
    -- looks like fmap that needs to be concated, realized after seeing solutions (see below)

tst1m = Bi 10 20 (Un 30)
tst2m = Bi 1 2 (Bi 3 4 (Un 5))

{-
course stuff solution

instance Functor OddC where
  fmap f (Un x) = Un (f x)
  fmap f (Bi x1 x2 r) = Bi (f x1) (f x2) (fmap f r)

instance Applicative OddC where
  pure x = Un x
  Un f <*> xs = fmap f xs
  Bi f1 f2 r <*> xs = concat3OC (fmap f1 xs) (fmap f2 xs) (r <*> xs)

instance Monad OddC where
  xs >>= k = concatOC (fmap k xs)

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un xs) = xs
concatOC (Bi xs ys (Un zs)) = concat3OC xs ys zs
concatOC (Bi xs ys (Bi zs1 zs2 r)) = concat3OC xs ys (concat3OC zs1 zs2 (concatOC r))

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC xs (Un y) zs = concat2OC xs y zs
concat3OC xs (Bi y1 y2 ys) zs = concat2OC xs y1 (concat3OC (Un y2) ys zs)

concat2OC :: OddC a -> a -> OddC a -> OddC a
concat2OC (Un x) y zs = Bi x y zs
concat2OC (Bi x1 x2 xs) y zs = Bi x1 x2 (concat2OC xs y zs)
-}
 
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
concat3OC (Un x1) (Bi y1 y2 u) c = Bi x1 y1 (concat3OC (Un y2) u c)
concat3OC (Bi x1 x2 u) b c = Bi x1 x2 (concat3OC u b c)

tst1 = Bi 'a' 'b' (Un 'c')
tst2 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
tst3 = Bi 'i' 'j' (Un 'k')
tst = concat3OC tst1 tst2 tst3 == Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))
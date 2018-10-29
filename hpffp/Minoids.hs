import Data.Monoid

newtype Mem s a = Mem {runMem :: s -> (a,s)}
instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ \s -> (mempty,s)
    mappend (Mem f1) (Mem f2) = 
        Mem $ \s -> let
            (r1,s1) = f1 s
            (r2,s2) = f2 s1
            in (r1 <> r2, s2)

f' = Mem $ \s -> ("hi", s + 1)
main = do
    print $ runMem (f' <> mempty) 0
    print $ runMem (mempty <> f') 0
    print $ (runMem mempty 0 :: (String, Int))
    print $ runMem (f' <> mempty) 0 == runMem f' 0
    print $ runMem (mempty <> f') 0 == runMem f' 0
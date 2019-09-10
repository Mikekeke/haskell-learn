class Functor w => Comonad w where
    (=>>) :: w a -> (w a -> b) -> w b
    extend :: (w a -> b) -> w a -> w b
    coreturn :: w a -> a
    cojoin :: w a -> w (w a)
    x =>> f = fmap f (cojoin x)
    extend = flip (=>>)
    -- extend f = fmap f . cojoin
    -- cojoin wa = extend id wa
    cojoin = extend id

data U x = U [x] x [x]

right (U a b (c:cs)) = U (b:a) c cs
left  (U (a:as) b c) = U as a (b:c)

instance Functor U where
    fmap f (U a b c) = U (map f a) (f b) (map f c)

instance Comonad U where
    cojoin a = U (tail $ iterate left a) a (tail $ iterate right a)
    coreturn (U _ b _) = b

rule (U (a:_) b (c:_)) = not (a && b && not c || (a==b))

shift i u = (iterate (if i<0 then left else right) u) !! abs i

toList i j u = take (j-i) $ half $ shift i u where
    half (U _ b c) = [b] ++ c

test = let u = U (repeat False) True (repeat False)
       in putStr $
          unlines $
          take 40 $
          map (map (\x -> if x then '#' else ' ') . toList (-20) 20) $
          iterate (=>> rule) u
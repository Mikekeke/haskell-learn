{-# LANGUAGE RankNTypes #-}

import Test.QuickCheck
import Control.Applicative hiding (Const, getConst)

-- https://vitez.me/building-lenses
newtype SimpleGetter s a = SimpleGetter (s -> a)


fst' :: SimpleGetter (a,b) a
fst' = SimpleGetter fst
tst1 = let (SimpleGetter f) = fst' in quickCheck (liftA2 (==) f fst :: (Int,Int) -> Bool)

view :: SimpleGetter s a -> s -> a
view (SimpleGetter f) = f
tst2 = view (SimpleGetter fst) (1, 2) == 1

infix 8 ^.
(^.) :: s -> SimpleGetter s a -> a
(^.) = flip view

toSimpleGetter :: (s -> a) -> SimpleGetter s a
toSimpleGetter = SimpleGetter
-- x ^. toSimpleGetter f ~ f x

lengthCont :: (Int -> r) -> String -> r
lengthCont = \c -> c . length
-- λ: lengthCont (*2) "asd" ~> 6

class Contravariant f where
    contramap :: (a -> b) -> f b -> f a

newtype MakeString a =
  MakeString { mkString :: a -> String }

instance Contravariant MakeString where
  contramap f (MakeString g) = MakeString (g.f)
--   contramap f = MakeString . (.f) . mkString
msExlcInt :: MakeString Int
msExlcInt = MakeString $ \a -> show a ++ "!"

msExlcBool :: MakeString Bool
msExlcBool = contramap fromEnum msExlcInt
-- λ: mkString msExlcBool True ~> "1!"

type Getter s a =
    forall f. (Contravariant f, Functor f)
    => (a -> f a) -> s -> f s

newtype Const a b = Const { getConst :: a } deriving Show

instance Functor (Const m)
  where fmap _ (Const a) = Const a

instance Contravariant (Const m)
  where contramap _ (Const b) = Const b

fstlGetter :: Getter (a,b) a
fstlGetter f t = contramap fst cf where
    cf = (f . fst $ t)

view' :: Getter (a, b) a -> (a, b) -> a  
view' getter s = getConst $ getter Const s

-- tt :: Getter (a,b) a
-- tt k (a,b) = fmap (\a' -> (a',b)) (k a) 
tt :: (a, t) -> Const a (b,t)
tt (a,b) = (Const a) 

-- не работает
ttl :: Getter [a] a
ttl k l = contramap head (k . head $ l) 


-- tplView :: (a, b) -> a
-- tplView = view' tt

view1 g s  = getConst (g Const s)

-- headGetter :: Getter [a] a
-- headGetter f t = contramap he:tad cf where
--     cf = (f . head $ t)

-- headV :: [a] -> a
-- headV = view1 headGetter

-- tst3 = view1 (headV . tplView )


{- well... the correct answer was just to write
(^.) :: s -> Getter s a -> a
s ^. g  = getConst (g Const s)
-}
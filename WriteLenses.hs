{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

import Test.QuickCheck
import Control.Applicative hiding (Const, getConst)
import Data.List

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

{- 
view' :: Getter (a,b) a -> (a,b) -> a  
view' getter s = getConst $ getter Const s

well... the correct answer was just to write polymorphic
(^.) :: s -> Getter s a -> a
s ^. g  = getConst (g Const s)
-}

view' :: Getter s a -> s -> a  
view' getter s = getConst $ getter Const s

fstGetter :: Getter (a,b) a
fstGetter fcf tpl = contramap fst $ fcf (fst tpl)

headGetter :: Getter [a] a
headGetter fcf = contramap head . fcf . head


nths :: Int -> [a] -> [a]
nths n l = let (_,r) = splitAt (pred n) l in 
    case r of 
      [] -> []
      (x:rest) -> x : nths n rest

thirds = nths 3

toGetter f fcf = contramap f . fcf . f

thirdsGetter :: Getter [a] [a]
thirdsGetter = toGetter thirds

tst3 = sequence_  [
    quickCheck (liftA2 (==) thirds (view' thirdsGetter) :: [Int] -> Bool)
  , quickCheck (liftA2 (==) fst (view' fstGetter) :: (Int,Int) -> Bool)
  ]



-- {-# LANGUAGE MultiParamTypeClasses #-} -- enabled by FunctionalDependencies
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

{-
task:
P(12) + P('1') should be P('121')
P(12) + P(2) should be P(14)

trying to replicate from telegram PONV (Mikhail):
case class P[A](value:A){ def +[B, Out](other:P[B])(implicit sum:Sum.Aux[A,B, Out]):P[Out] = sum(value, other.value) }

trait Sum[A,B]{ type Out;  def apply(a:A, b:B):Out }

object Sum {
   type Aux[A,B,Out0] = Sum[A,B] { type Out = Out0 }

   implicit def sumIntString:Aux[Int,String,String] = new Sum …
}

-}

newtype P a = P a deriving Show

p1 :: P Integer
p1 = P 1
p2 :: P Integer
p2 = P 2
p3 :: P String
p3 = P "10"

class Sumbl a b c | a b -> c where
    sm :: a -> b -> c

instance Sumbl (P Integer) (P String) (P String)  where
    sm (P x) (P y) = P $ show x ++ y

instance Sumbl (P Integer) (P Integer) (P Integer) where
    sm (P x) (P y) = P $ x + y

data Showable = forall a . Show a => ConsSh a
instance Show Showable where
    show (ConsSh a) = '(' : shows a ")" 

pack :: Show a => a -> Showable
pack = ConsSh

t1 = sm p1 p2
t2 = sm p1 p3
tests = [pack t1, pack t2] -- [(P 3),(P "110")]

{-
another approach seen at
https://www.youtube.com/watch?v=xVX_SAckF7Q at ~23:20
-}

class SymblTwo a b where
    type SumTwo a b  -- TypeFamilies
    smt :: a -> b -> SumTwo a b

instance SymblTwo (P Integer) (P String) where
    type SumTwo (P Integer) (P String) = (P String)
    smt (P x) (P y) = P $ show x ++ y

instance SymblTwo (P Integer) (P Integer)  where
    type SumTwo (P Integer) (P Integer) = (P Integer)
    smt (P x) (P y) = P $ x + y

t3 = smt p1 p2
t4 = smt p1 p3
tests2 = [pack t1, pack t2] -- [(P 3),(P "110")]

testAll = show tests == show tests2


-- not so polymorphic
class Sumbl2 a b  where
    st :: a -> b -> b

instance Sumbl2 (P Integer) (P String) where
    st (P x) (P y) = P $ show x ++ y

t5 = st p1 p3
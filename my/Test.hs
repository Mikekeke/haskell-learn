module Test where

product' :: Num t => [t] -> t
product' []     = 1
product' (x:xs) = x * product xs

ff = \(a,b) -> a

f1 x = x+2
f2 y = y*y

f3 x = f2(f1 x)

ff2 :: [Integer]
ff2 = map (+1)([1,2,3])

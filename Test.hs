module Test where

import Data.Char
import Data.Function

devideTwoBy = (2 /)
devidedByTwo = (/ 2)

sinHalfPiUsual = sin (pi / 2)
sinHalfPiNoParnts = sin $ pi / 2
sinHalfPiFuckedUp = sin pi / 2

testDigit :: Char -> Bool
testDigit c = isDigit c

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int c1 c2 =
    if isDigit c1 && isDigit c2
    then digitToInt c1 * 10 + digitToInt c2
     else 100

tupling1 = fst("one", 2)
tupling2 = snd("one", 2)

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt $ (fst p2 - fst p1) ^ 2 + (snd p2 - snd p1) ^ 2


fact1 0 = 1
fact1 n = if n <= 0 then error "n must be >= 0" else n * fact1(n - 1)

-- guards
fact2 0 = 1
fact2 n | n < 0 = error "n must be >= 0"
        | n > 0 = n * fact2(n - 1)


fact3 :: Integer -> Integer
fact3 n | n == 0    = 1
        | n > 0     = n * fact3(n - 1)
        | otherwise = error "n must be >= 0"

fib n
  | n == 0 = 0
  | n == 1 = 1
  | n < 0 = -(-1) ^ (-n) * fib (-n)
  | n > 0 = fib (n - 1) + fib (n - 2)

-- fib 4 = fib 3 + fib 2
-- fib 4 = (fib 2 + fib 1) + (fib 1 + fib 0)
-- fib 4 = ((fib 1 + fib 0) + 1) + (1 + 0)

fact4 n | n >= 0 = loop 1 n
        | otherwise = error "n >= 0 !"

loop acc 0 = acc
loop acc n = loop (acc * n) (n - 1)


fibTail n | n == 0 = 0
          | n == 1 = 1
          | n > 1 = fibLoop 0 1 (n - 2)
          | n < 0 = -(-1) ^ (-n) * fibTail (-n)
          where
          fibLoop curr next iter | iter == 0 = curr + next
                                 | otherwise = fibLoop(next)(curr + next)(iter - 1)


syntax1 a b =
    let {x = 10; a1 = a + x; b1 = b + x}
    in (a1, b1)

syntax2 a b =
    let
      x = 10
      a1 = half a + x
      b1 = b + x
      half = (/ 2)
    in (a1, half b1)

-- :set +s to print time and memory
sample = (let x = 'w' in [x,'o',x]) ++ "!"

fact5 n | n >= 0 = let
          loop acc 0 = acc
          loop acc n = loop (acc * n) (n - 1)
        in loop 1 n
        | otherwise = error "n >= 0 !"

minusTupl :: Num a => (a, a) -> a
minusTupl t =
    let
      (a,b) = t
    in a - b

sum'n'countMy :: Integer -> (Integer, Integer)
sum'n'countMy x
  |x == 0 = (1, 0)
  |x > 0 = countNsum x 0 0
  |x < 0 = sum'n'count (-x)
  where
    countNsum x1 sum cnt
      |x1 == 0 = (sum, cnt)
      |otherwise = countNsum (div x1 10) (sum + (mod x1 10)) (cnt + 1)


sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x
  | x == 0 = (0, 1)
  | x < 0 = iter 0 0 (-x)
  | otherwise = iter 0 0 x
  where
    iter sum count 0 = (sum, count)
    iter sum count x = let
        (x', d) = divMod x 10
      in iter (sum + d) (count + 1) x'

-- begin 1
multSecond = g `on` h

g :: Num a => a -> a -> a
g a b = a * b

h :: (a, b) -> b
h tuple = snd tuple
--GH?Ci> multSecond ('A',2) ('E',7)
--14
--end 1

--begin 2
on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)

--GHCi> let sum3squares = (\x y z -> x+y+z) `on3` (^2)
--GHCi> sum3squares 1 2 3
--14
--end 2
module Test where

import Data.Char

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

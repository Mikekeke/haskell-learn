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
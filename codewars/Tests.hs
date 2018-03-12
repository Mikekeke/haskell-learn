module Tests where

import           Control.Applicative
import           Data.List
import           Data.Ord
import           GHC.Exts
import           Text.Printf

printerError :: [Char] -> [Char]
printerError s =
    let
        valid = ['a'..'m']
        errors = foldl (\b a -> if elem a valid then b+1 else b) 0 s
    in
        show errors ++ "/" ++ show (length s)

--best on site
printerError' :: [Char] -> [Char]
printerError' = printf "%d/%d" <$> length . filter (> 'm') <*> length

-- ************************************************
testarr =  ["zone", "abigail", "theta", "form", "libe", "zas", "theta", "abigail"]

grpHelper' _ [] = []
grpHelper' nn l
        | nn > (length l) = []
        | otherwise = (concat $ take nn l) : grpHelper' nn (tail l)


longestConsec :: [String] -> Int -> String
longestConsec strarr k
  | n == 0 || k > n || k <= 0 = ""
  | otherwise = head . sortWith ((0-) <$> length) $ grpHelper strarr -- that's my shit
--   | otherwise = maximumBy (comparing length) $ grpHelper strarr -- but result reversed in my case
    where
      n = length strarr
      grpHelper :: [String] -> [String]
      grpHelper [] = []
      grpHelper l
        | k > (length l) = []
        | otherwise = (concat $ take k l) : grpHelper (tail l)

{-# LANGUAGE LambdaCase #-}
import Control.Monad
import Control.Applicative
import Data.List

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk x l = (take x l) : (chunk x . drop x $ l)

testGrid = [
     "..."
    ,".O."
    ,"..."
    ]

parseTile = \case {'.' -> 0; 'O' -> 3}

parseGrid :: [[Char]] -> (Int, [Int])
parseGrid = liftA2 (,) (length . head) (map parseTile . join)

showTile :: Int -> Char
showTile 0 = '.'
showTile x | x `elem` [1,2,3] = 'O'

showGrid :: Int -> [Int] -> [[Char]]
-- showGrid side greed = (fmap showTile) <$> chunk side greed 
-- showGrid side = ((fmap showTile) <$>) . chunk side 
showGrid = ((fmap showTile <$>) .) . chunk

bomberMan n grid = undefined



tsts = zip [1..] [ 
    (showGrid 3 $ snd $ parseGrid testGrid) == testGrid
    ]

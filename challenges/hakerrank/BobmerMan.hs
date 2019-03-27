{-# LANGUAGE LambdaCase #-}
-- import Control.Monad
import Control.Applicative
import Data.List
import Control.Monad.State

type Pos = Integer
type Obj = Int
type Tile = (Pos, Obj)
type Grid = [Tile]

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk x l = (take x l) : (chunk x . drop x $ l)

byXY :: [a] -> Int -> Int -> Int -> a
byXY l n x y = l !! (y*rowsCount + x) where 
    rowsCount = length l `div` n

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
showTile x | x `elem` [2,3] = 'O'

showGrid :: Int -> [Int] -> [[Char]]
-- showGrid side greed = (fmap showTile) <$> chunk side greed 
-- showGrid side = ((fmap showTile) <$>) . chunk side 
showGrid = ((fmap showTile <$>) .) . chunk

testPrsdGrd = zip [0..] $ snd $ parseGrid testGrid

tickTile :: Tile -> State [Integer] Tile
tickTile t@(_, 0) = return t 
tickTile (i, 1) = modify (i:) >> return (i,0) 
tickTile t = return (pred <$> t) 
-- tickTile x = pred x

-- runRound :: Grid -> State [Int] Grid
-- runRound g  = do
--     exp <- get
--     undefined
    
--     let nextGrid = foldr f ([],[]) where
--         f pt (expl, ng) = case snd pt of
--             1 -> (fst pt : expl, (tickTile <$> pt) : newngGrid)
--             _ -> (expl, pt : newGrid)

bomberMan n grid = undefined



tsts = zip [1..] [ 
    (showGrid 3 $ snd $ parseGrid testGrid) == testGrid
    , let byXYL =  byXY [1,2,3,4,5,6,7,8,9] 3 in byXYL 0 0 == 1 && byXYL 1 1 == 5 && byXYL 0 2 == 7
    ]

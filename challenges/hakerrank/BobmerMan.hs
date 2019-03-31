{-# LANGUAGE LambdaCase #-}
import Control.Monad
import Control.Applicative
import Data.List
import Control.Monad.State
import Debug.Trace
import qualified Data.IntSet as IS (IntSet, fromList, unions,union, filter, member )

type Pos = Int
type Obj = Int
type Tile = (Pos, Obj)
type Grid = [Tile]
type GridWidth = Int

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk x l = (take x l) : (chunk x . drop x $ l)

byXY :: [a] -> Int -> Int -> Int -> a
byXY l n x y = l !! (y*rowsCount + x) where 
    rowsCount = length l `div` n

testGrid = [
     "O..O"
    ,".O.."
    ,"...."
    ]



parseTile = \case {'.' -> 0; 'O' -> 3}

parseGrid :: [[Char]] -> (Int, [Int])
parseGrid = liftA2 (,) (length . head) (map parseTile . join)

-- showTile :: Int -> Char
-- showTile 0 = '.'
-- showTile x | x `elem` [1,2,3] = 'O'

showTile :: Int -> Char
showTile 0 = '0'
showTile 1 = '1'
showTile 2 = '2'
showTile 3 = '3'

showGrid :: Int -> [Int] -> [[Char]]
-- showGrid side greed = (fmap showTile) <$> chunk side greed 
-- showGrid side = ((fmap showTile) <$>) . chunk side 
showGrid = ((fmap showTile <$>) .) . chunk

ppGrid :: GridWidth -> Grid -> String
ppGrid gw g = foldMap (++"\n") $ chunk gw $ snd <$> (fmap showTile) <$> g

testPrsdGrd = zip [0..] $ snd $ parseGrid testGrid

tickTile :: Tile -> ([Int], Tile)
tickTile t@(_, 0) = ([], t) 
tickTile (i, 1) = ([i], (i,0)) 
tickTile t = ([], pred <$> t) 

plantBombs :: Grid -> Grid
plantBombs = fmap (\(i,x) -> if x == 0 then (i,3) else (i,x))

calcCleared :: GridWidth -> Int -> [Int]
calcCleared gw x = [x, succ x, pred x, x + gw, x - gw]

clearTile :: Tile -> Tile
clearTile = fmap (const 0)

clearBlown :: GridWidth -> [Int] -> Grid -> Grid
clearBlown _ [] g = g
clearBlown gw bIxs g = let blownSet = IS.fromList $ bIxs >>= calcCleared gw
                           clearFun :: Tile -> Grid -> Grid
                           clearFun t = if (fst t) `IS.member` blownSet then (clearTile t :)  else (t:)
                        in foldr clearFun [] g

{-
3 0 0 3        2 0 0 2        1 3 3 1

0 3 0 0  ->    0 2 0 0  ->    3 1 3 3

0 0 0 0        0 0 0 0        3 3 3 3
-}


runRound :: GridWidth ->  State (Int ,Grid) [Int]
runRound gw = do
    (expl, ng) <- gets $ traverse tickTile . snd
    modify (\(step, _) -> (succ step, ng))
    step <- gets fst
    when (even step) (modify $ fmap plantBombs)
    modify (fmap $ clearBlown gw expl)
    get >>= \(step',cg') -> traceM ("\nstep: " ++ show step' ++ ", planting: " ++ show (even step') ++ "\ngrid:\n" ++ ppGrid gw cg')
    return expl
    
bomberMan n grid = let (gw, rawG) = parseGrid grid
                       gr :: Grid
                       gr = zip [1..] rawG
                       st = runRound gw
                   in runState (replicateM n st) (0, gr)



tsts = zip [1..] [ 
    (showGrid 3 $ snd $ parseGrid testGrid) == testGrid
    , let byXYL =  byXY [1,2,3,4,5,6,7,8,9] 3 in byXYL 0 0 == 1 && byXYL 1 1 == 5 && byXYL 0 2 == 7
    ]

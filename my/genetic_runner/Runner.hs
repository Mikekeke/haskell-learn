{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.List
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe
import Control.Applicative
import System.Random
import Debug.Trace

{- types and data -}
data Tile = Flat | Rock | Peak deriving Show
type Track = [Tile]
data Move = Run | Jump | DoubleJump deriving Show
data Runner = Runner {energy :: Int, moveSet :: [Move]} deriving Show
data RunnerRank = RunnerRank {runnerId :: String, rank :: Int} deriving Show
type RunState = (Track, Runner)
data RunResult = NoMovesLeft | NoEnergyLeft | TrackEnded | NoEnergyTo Move deriving Show
newtype RunnerApp a = RunnerApp {unApp :: ExceptT RunResult (State RunState) a} 
    deriving (Functor, Applicative, Monad, MonadState RunState, MonadError RunResult)

runApp :: RunnerApp a -> RunState -> (Either RunResult a, RunState)
runApp = runState . runExceptT . unApp

{- execution step related -}
tickLstWthErr :: RunResult -> (a -> [b]) -> a -> RunnerApp (b, [b])
tickLstWthErr err getter obj | (m:ms) <- getter obj = return (m,ms)
                             | otherwise = throwError err
tickMoveSet ::Runner -> RunnerApp (Move, [Move])
tickMoveSet  = tickLstWthErr NoMovesLeft moveSet
tickTrack :: Track -> RunnerApp (Tile, Track)
tickTrack = tickLstWthErr TrackEnded id

requiredEnergy Run        = 1
requiredEnergy Jump       = 2
requiredEnergy DoubleJump = 3

tickRunner :: Runner -> RunnerApp (Move, Runner)
tickRunner rnr = do
    (currMv, nextMs) <- tickMoveSet rnr
    let newEnergy = energy rnr - requiredEnergy currMv
    when (newEnergy < 0) (throwError $ NoEnergyTo currMv)
    return $ (currMv, rnr {energy = newEnergy, moveSet = nextMs})


canPass :: Move -> Tile -> Bool 
canPass Run Flat = True
canPass Jump Rock = True
canPass DoubleJump Peak = True
canPass _ _ = False

trackEnded :: Track -> Bool
trackEnded = null

tryTrack :: Track -> Move -> RunnerApp Track
tryTrack tr move = do
    (nextT, _) <- tickTrack tr
    let nextTrack = if canPass move nextT then tail tr else tr
    return nextTrack

makeMove ::  RunnerApp ()
makeMove = do
    (tr, rnr) <- get
    (currMv, nextRnr) <- tickRunner rnr
    nextTr <- tryTrack tr currMv
    put (nextTr, nextRnr)

{- experiment data generating -}
genMoveSet :: IO [Move]
genMoveSet = do
    mvsN <- randomRIO (6,20)
    mvsIds <- (replicateM mvsN $ randomRIO (1,3) :: IO [Int])
    return $ map pickMove mvsIds

genEnergy :: IO Int
genEnergy = randomRIO (6,30)

genRunner :: IO Runner
genRunner = liftA2 Runner genEnergy genMoveSet
genRunners n = replicateM 20 genRunner

pickMove :: Int -> Move
pickMove 1 = Run
pickMove 2 = Jump
pickMove 3 = DoubleJump

{- breed logic -}
getPercent :: IO Int
getPercent = randomRIO (0,100)

getRandMv = pickMove <$> randomRIO (1,3)

get50Chance :: IO Bool
get50Chance = toEnum <$> randomRIO (0,1)

pickAlt v1 v2 p = case p of
    True -> v1
    False -> v2

mutated :: Int -> Int -> Bool
mutated mr prcnt = prcnt <= mr

crossEnergy mr e1 e2 = do
    mtd <- (mutated mr) <$> getPercent
    diffRir <- (pickAlt (-) (+)) <$> get50Chance
    let diff = abs $ e1 - e2
        av = (e1 + e2) `div` 2
    return $ pickAlt (diffRir av (div diff 2)) av mtd

subCross _ _ [] = pure []
subCross _ 0 _ = pure []
subCross mr n (x:xs) = do
    mtd <- (mutated mr) <$> getPercent
    nxtMv <- pickAlt (pure x) getRandMv mtd
    (nxtMv:) <$> subCross mr (n-1) xs

-- subCross' :: Int -> Int -> [Move] -> IO [Move]
subCross' :: Int -> [Move] -> IO [Move]
subCross' n ms = foldr g (const $ pure []) ms n where
    g :: Move -> (Int -> IO [Move]) -> Int -> IO [Move]
    g m rf i = case i of 
        0 -> pure []
        _ -> (m:) <$> rf (i - 1)
    
{-
[Run,Run,Run]
\n -> case n... <$> (case  n-1... <$> (case n-2... <$> (case  n-3... (\_ -> pure []))))
\10 -> case 10... <$> (case 9... <$> (case 8... <$> (case 7...  (\_ -> pure [])))
(Run:) <$> ((Run:) <$> ((Run:) <$> (pure [])))
\1 -> case 1... <$> (case 0...)
(Run:) <$> (pure []) - правое значение (rf) далее не вычисляется
-}

crossMvSet :: Int -> [Move] -> [Move] -> IO [Move]
crossMvSet _ [] [] = pure []
crossMvSet mr (x:xs) [] = do
    mtd <- (mutated mr) <$> getPercent
    nxtMv <- pickAlt (pure x) getRandMv mtd
    (nxtMv:) <$> crossMvSet mr xs []

crossMvSet mr (x:xs) (y:ys) = do
    mtd <- (mutated mr) <$> getPercent
    let crossMv = (pickAlt x y) <$> get50Chance
        mutMv = getRandMv
    nxtMv <- pickAlt crossMv mutMv mtd
    (nxtMv:) <$> crossMvSet mr xs ys

crossRunners mutRate r1 r2 = do
    en <- crossEnergy mutRate (energy r1) (energy r2)
    ms <- crossMvSet mutRate (moveSet r1) (moveSet r2)
    return $ Runner en ms
    
tstCrossRunnersKek times = (fmap . fmap . fmap $ sequenceA) fn where 
    fn = ((fmap sequenceA) <$> (sequenceA <$> (sequenceA $ replicate times crossRunners)))

tstCrossRunners times mr r1 r2 = replicateM times $ crossRunners mr r1 r2
-- p-free: tstCrossRunners times = (:(replicateM times .) .) . crossRunners
-- tstCrossRunners times mr r1 r2 = let res = crossRunners mr r1 r2 in sequenceA $ replicate times $ res --!!! results here will be random too
{-
λ: let rnd = randomRIO (0,100) in sequenceA $ replicate 3 rnd
or
λ: let rnd = randomRIO (0,100) in replicateM 3 rnd
[12,74,33]
-}

tstCrossRunners1 = 
    let
        r1 = Runner 5 [Run,Run,Run]
    in tstCrossRunners 10 50 r1 r1 >>= mapM_ (putStrLn . show)


{- experiment logic -}

-- runTrack :: Track -> Runner -> (Either RunResult (), RunState)
runTrack tr rnr = runApp (forever makeMove) (tr, rnr)

testTrack = [Flat, Flat, Peak, Rock, Flat, Rock]

runTestTrack :: Runner -> (Either RunResult (), RunState)
runTestTrack = runTrack testTrack


main :: IO ()
main = do
    runners <- genRunners 20
    mapM_ (putStrLn . show . ap (,) runTestTrack) runners


{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.List
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe
import Control.Applicative
import System.Random

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

pickMove 1 = Run
pickMove 2 = Jump
pickMove 3 = DoubleJump

{- experiment logic -}

-- runTrack :: Track -> Runner -> (Either RunResult (), RunState)
runTrack tr rnr = runApp (forever makeMove) (tr, rnr)

testTrack = [Flat, Flat, Peak, Rock, Flat, Rock]

runTestTrack :: Runner -> (Either RunResult (), RunState)
runTestTrack = runTrack testTrack


main :: IO ()
main = do
    runners <- replicateM 20 genRunner
    mapM_ (putStrLn . show . ap (,) runTestTrack) runners


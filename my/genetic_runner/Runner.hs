{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.List
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe
import Control.Applicative

data Tile = Flat | Rock | Peak deriving Show
type Track = [Tile]
data Move = Run | Jump | DoubleJump deriving Show
data Runner = Runner {rID :: String, energy :: Int, moveSet :: [Move]} deriving Show
data RunnerRank = RunnerRank {runnerId :: String, rank :: Int} deriving Show
type RunState = (Runner, [Tile], RunnerRank)

testTrack = [Flat, Flat, Peak, Rock, Flat]
testRunner = Runner {
    rID = "r1"
    , energy = 6
    , moveSet = [Run, Run, Run, Run, Run, Run, Run, Run]
}
initRank runner = RunnerRank (rID testRunner) 0
testState = (testRunner, testTrack, initRank testRunner)

tickMoveset rnr = case moveSet rnr of
    [] -> Left "Runner no moves"
    m:ms -> Right (m,ms)

tickEnergy rnr = let nxt = pred (energy rnr) in case nxt of
    0 -> Left "Runner no energy"
    x -> Right x

tickTrack tr = case tr of
    [] -> Left "Track ended"
    t:ts -> Right (t,ts)

canPass :: Move -> Tile -> Bool 
canPass Run Flat = True
canPass Jump Rock = True
canPass DoubleJump Peak = True
canPass _ _ = False

trackEnded :: [Tile] -> Bool
trackEnded = null

-- makeStep rnr tr = do
--     nextEnrg <- tickEnergy rnr
--     (nextM, nextMs) <-  tickMoveset rnr
--     (nextT, restT) <- tickTrack tr
--     let nextTrack = if canPass nextM nextT then restT else tr
--     return (rnr {energy = nextEnrg, moveSet = nextMs}, nextTrack)


-- todo: better validated here
tickRunner rnr = do
    nextEnrg <- tickEnergy rnr
    (_, nextMs) <-  tickMoveset rnr
    return $ rnr {energy = nextEnrg, moveSet = nextMs}

upRank rnk = rnk {rank = succ $ rank rnk}

tryTrack rnr tr = do
    (nextM, _) <- tickMoveset rnr
    (nextT, _) <- tickTrack tr
    let nextTrack = if canPass nextM nextT then tail tr else tr
    return nextTrack

changeState (runner, track, rnk) = liftA3 (,,) (tickRunner runner) (tryTrack runner track) (pure $ upRank rnk)

newtype RunnerApp a = RunnerApp {unApp :: ExceptT String (State (Runner, Track, RunnerRank)) a} 
    deriving (Functor, Applicative, Monad, MonadState RunState, MonadError String)
runApp = runState . runExceptT . unApp

makeMove :: RunnerApp ()
makeMove = get >>= either throwError put . changeState

test1 = runApp (forever makeMove) testState
test2 = runApp (replicateM 4 makeMove) testState



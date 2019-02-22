import Control.Monad.State
import Data.List
import Control.Monad.Except

data Tile = Flat | Rock | Peak
data Move = Run | Jump | DoubleJump
data Runner = Runner {rID :: String, energy :: Int, moveSet :: [Move]}
data RunnerRank = RunnerRank {runnerId :: String, rank :: Int}

testTrack = [Flat, Flat, Peak, Rock, Flat]
testRunner = Runner {
    rID = "r1"
    , energy = 6
    , moveSet = [Run, Run, DoubleJump, Jump, Run]
}

canRun runner | energy runner == 0 = False
              | otherwise = True

trackEnded :: [Move] -> Bool
trackEnded = null

type RunApp = ExceptT String (State (Runner, [Move], RunnerRank)) ()
makeMove :: RunApp
makeMove = do
    (runner, track, rank) <- get
    return undefined


attempt :: State (Runner, [Move], RunnerRank) () 
attempt = do
    (runner, track, rank) <- get
    return undefined

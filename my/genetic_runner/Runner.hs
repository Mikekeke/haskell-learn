import Control.Monad.State
import Data.List
import Control.Monad.Except
import Control.Monad

data Tile = Flat | Rock | Peak deriving Show
data Move = Run | Jump | DoubleJump deriving Show
data Runner = Runner {rID :: String, energy :: Int, moveSet :: [Move]} deriving Show
data RunnerRank = RunnerRank {runnerId :: String, rank :: Int} deriving Show

testTrack = [Flat, Flat, Peak, Rock, Flat]
testRunner = Runner {
    rID = "r1"
    , energy = 6
    , moveSet = [Run, Run, DoubleJump, Jump, Run]
}
initRank runner = RunnerRank (rID testRunner) 0
testState = (testRunner, testTrack, initRank testRunner)

runnerDone runner | energy runner == 0 = True
                  | otherwise = False

trackEnded :: [Tile] -> Bool
trackEnded = null

changeState (runner, track, currentRank) = (
    runner {energy = (pred $ energy runner), moveSet = tail (moveSet runner)}
    , tail track
    , currentRank {rank = succ $ rank currentRank} )

type RunnerApp = ExceptT String (State (Runner, [Tile], RunnerRank)) ()
makeMove :: RunnerApp
makeMove = do
    (runner, track, rank) <- get
    when (trackEnded track) (throwError "Track ended")
    when (runnerDone runner) (throwError "Runner done")
    modify changeState


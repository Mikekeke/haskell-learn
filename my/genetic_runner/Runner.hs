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

testTrack = [Flat, Flat, Peak, Rock, Flat]
testRunner = Runner {
    rID = "r1"
    , energy = 6
    , moveSet = [Run, Run, Run, Run]
}
initRank runner = RunnerRank (rID testRunner) 0
testState = (testRunner, testTrack, initRank testRunner)

noEnergy rnr | energy rnr == 0 = True
             | otherwise = False
noMoves = null . moveSet

nextMove rnr = case moveSet rnr of
    [] -> Left "No moves left"
    x:_ -> Right x
nextTile tr = case tr of
    [] -> Left "Track ended"
    x:_ -> Right x

canPass :: Move -> Tile -> Bool 
canPass Run Flat = True
canPass Jump Rock = True
canPass DoubleJump Peak = True
canPass _ _ = False

trackEnded :: [Tile] -> Bool
trackEnded = null

tickRunner rnr = rnr {energy = (pred $ energy rnr), moveSet = tail (moveSet rnr)}
tickRank cr = cr {rank = succ $ rank cr}

tryTrack rnr tr = do
    nextM <- nextMove rnr
    nextT <- nextTile tr
    let nextTrack = if canPass nextM nextT then tail tr else tr
    return nextTrack

changeState (runner, track, currentRank) = tryTrack runner track
    
    -- (
    -- runner {energy = (pred $ energy runner), moveSet = tail (moveSet runner)}
    -- , undefined
    -- , currentRank {rank = succ $ rank currentRank} )

-- type RunnerApp = ExceptT String (State (Runner, Track, RunnerRank)) ()
-- runApp = runState . runExceptT
-- makeMove :: RunnerApp
-- makeMove = do
--     (runner, track, rank) <- get
--     when (trackEnded track) (throwError "Track ended")
--     when (noEnergy runner || noMoves runner) (throwError $ "Runner done: " ++ show runner)
--     modify changeState


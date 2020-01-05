import Data.List
import qualified Data.Set as Set
import Debug.Trace
import Data.Function

data RPS = Rock | Papper | Scissors deriving (Eq, Show)
instance Ord RPS where
    Rock `compare` Scissors = GT
    Rock `compare` Papper = LT
    Rock `compare` Rock = EQ
    Scissors `compare` Papper = GT
    Scissors `compare` Rock = LT
    Scissors `compare` Scissors = EQ
    Papper `compare` Rock = GT
    Papper `compare` Scissors = LT
    Papper `compare` Papper = EQ

type Result = (String, RPS)
getWinner :: (Result,Result,Result) -> String
getWinner (ans1, ans2, ans3) = 
    let
        threeDraw = (Set.size  . Set.fromList . fmap snd $ [ans1, ans2, ans3]) == 3
        processeRes = take 2 . sortBy (flip compare `on` snd)
        findWinner [a,b] | snd a == snd b = mconcat ["Draw between ", fst a, " and ", fst b]
                         | otherwise =  fst a ++ " wins!"
        findWinner _ = error "WTF"
    in case threeDraw of
        True -> "Three draw"
        False -> findWinner $ processeRes [ans1, ans2, ans3]
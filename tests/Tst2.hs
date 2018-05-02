import           Control.Monad.State

data Terrain = Flat | Rock
data AntState =  Fresh | Tired | Dead deriving Show
data Ant = Ant {pow :: Int, st :: AntState} deriving Show

testTerrain = [Flat, Flat, Rock, Rock, Flat]

-- evalState :: State s a -> s -> a
-- execState :: State s a -> s -> s
-- runState :: State s a -> s -> (a, s)

calcState :: Int -> AntState
calcState power = if power == 0 then Tired else Fresh

tireAnt (Ant p Fresh)
    | p > 0 = let newPower = p-1 in Ant newPower (calcState newPower)
    | otherwise = error "Illegal ant"
tireAnt (Ant 0 Tired) = Ant 0 Dead
tireAnt (Ant _ _) = error "Illegal ant"

behavior :: Terrain -> State Ant ()
behavior Flat = modify id
behavior Rock = modify tireAnt

test :: [Terrain] -> State Ant ()
-- test []     = do modify id -- works too
test []     = modify id
-- test (x:[]) = do behavior x -- works too
test (x:[]) = behavior x
test (x:xs) = do
    behavior x
    test xs

runTest1 = runState . test $ testTerrain

behavior' :: Terrain -> State Ant Ant
behavior' Flat = get
behavior' Rock = modify tireAnt >> get

-- test2 :: [Terrain] -> StateT Ant Data.Functor.Identity.Identity [Ant] -- whatever...
test2 terrain = sequence (map behavior' terrain)
runTest2 :: Ant -> ([Ant], Ant)
runTest2 = runState . test2 $ testTerrain

behaviorSimple :: Ant -> Terrain -> Ant
behaviorSimple ant Flat = ant
behaviorSimple ant Rock = tireAnt ant
-- runTest3 ant = foldl behaviorSimple ant testTerrain
runTest3 = flip (foldl behaviorSimple) testTerrain

-- add output for some ants and move to some file with normal name




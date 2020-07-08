module CodewarRanking where
    
data User = User Int Int deriving Show

newUser :: User
newUser = User (-8) 0

rank :: User -> Int
rank (User r _) = r

progress :: User -> Int
progress (User _ p) = p

levelCap = 100

rankDiff r1 r2 = let r' = r1 - r2 in if r2 < 0 && (r' + r2) >= 0 then r' - 1 else r'

calcDelta :: Int -> Int -> Int -> (Int,Int)
calcDelta taskRating  userRating currProgress = let
        score = case compare taskRating userRating of
            GT -> calcHarderScore taskRating userRating
            EQ -> 3
            LT -> calcEasierScore userRating taskRating
    in divMod (score + currProgress) levelCap

calcHarderScore taskRating userRating = let diff = rankDiff taskRating userRating in 10 * diff * diff
calcEasierScore taskRating userRating = case rankDiff taskRating userRating of
        1 -> 1
        _ -> 0

upRank :: Int -> Int -> Int
upRank _ 8 = 8 
upRank earnedRanks currRank = if rank'' >= 8 then 8 else rank''
    where
        rank' = earnedRanks + currRank
        rank'' = if currRank < 0 && rank' >= 0 then rank' + 1 else rank'

incProgress :: Int -> User -> User
incProgress activityRank u@(User rnk prg)
    | activityRank < (-8) || activityRank > 8 || activityRank == 0 = error "Bad task rank"
    | rnk == 8 = u
    | otherwise = 
        let 
            (earnedRanks, newProg) = calcDelta activityRank rnk prg
            newRank = upRank earnedRanks rnk
            newProg' = if newRank == 8 then 0 else newProg
        in User newRank newProg'
    
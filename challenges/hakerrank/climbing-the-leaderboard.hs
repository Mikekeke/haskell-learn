import Data.List
-- well... I tried, fails on running
fn res _ _ [] = res
fn res top [] (_:as)  = fn (head res : res) top [] (tail as)
fn res top scs (a:as) = let
    (higher, lower) = partition (>a) scs
    (same, nonSame) = partition (== a) $ (a:as)
    newTop = top + length higher
    nextRank = case null res of 
        True -> (length higher) + 1
        False -> newTop + 1
    newRes = (replicate (length same) nextRank) ++ (res `seq` res)
    in fn newRes newTop lower nonSame

climbingLeaderboard scores alice = fn [] 0 sc al where
    sc = map head . group $ scores
    al = reverse alice

-- from solutions (thats what I tried to implement)
solve1 scores' alice' = reverse $ go 1 scores alice
  where scores = map head $ group scores' 
        alice = reverse alice'
        go _ _ [] = []
        go k [] (a:as) = k : go k [] as 
        go k (s:ss) (a:as)
          | a >= s  = k : go k (s:ss) as
          | a < s  = go (k+1) ss (a:as)

solve2 :: [Int] -> [Int] -> [String]
solve2 s l = reverse . map show $ ranking ranks levels
  where (scores:levels:_) = [s,l]
        ranking = ranking' []
        ranking' rs _ [] = rs
        ranking' rs [] lz = [1 | l<-lz]++rs
        ranking' rs ((r,s):rnks) (l:lz)
          | s == l = ranking' (r:rs) rnks lz
          | s < l = ranking' rs rnks (l:lz)
          | otherwise = ranking' ((r+1):rs) ((r,s):rnks) lz
        ranks = reverse . zip [1..] . map head $ group scores
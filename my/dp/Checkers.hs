import           GHC.Arr         as A
import Debug.Trace
n = 25
nextPos (r,c) = ((pred r, succ c), (succ r, succ c))
rowIsOk = (\x -> x > 0 && x <= n) . fst

solve = curry go where
    go :: (Int, Int) -> [[(Int, Int)]]
    go pos | not (rowIsOk pos) = []
           | snd pos == n = [[pos]]
           | otherwise = let 
                            (p1, p2) = nextPos pos
                            -- moved up to "go" defenition
                            -- next1 = if rowOk p1 then go p1 else [[]]
                            -- next2 = if rowOk p2 then go p2 else [[]]
                         in  fmap (pos:) (go p1 ++ go p2)

solveMem = curry go where
    go :: (Int, Int) -> [[(Int, Int)]]
    go pos  | snd pos == n = [[pos]]
            | otherwise = let (p1, p2) = nextPos pos
                              nxt p = if rowIsOk p then memArr ! p else []
                          in  fmap (pos:) (nxt p1 ++ nxt p2)
    bounds = ((1,1), (n,n))
    memArr = A.listArray (bounds) (map go (A.range bounds))

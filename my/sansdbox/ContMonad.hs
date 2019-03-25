import Control.Monad.Cont
import Control.Monad

cnt1 :: Int -> Cont r Int
cnt1 x = return $ (+1) x

tst1 = runCont (replicateM 4 (cnt1 1)) id -- [2,2,2,2]
tst2 =  runCont (foldr1 (>=>) (replicate 4 cnt1) 1) id -- 5

-- https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style
chainCPS :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
chainCPS c1 c2 = \c -> c1 (\a -> c2 a c)
-- answer: chainCPS s f = \k -> s $ \x -> f x $ k


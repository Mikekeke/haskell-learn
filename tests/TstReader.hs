import Control.Monad.Trans.Reader


tstR1 ::Reader [String] [Int]
tstR1 = do
    ss <- asks $ ("ddddd" :) 
    return $ map length ss
    
    
f = const ["ddddd"] 
tst1 = runReader tstR1 ["a", "bb", "ccc"]
tst2 = runReader (local f tstR1) ["a", "bb", "ccc"]

tstR2 ::Reader [String] [Int]
tstR2 = do
    ss <- local f ask
    return $ map length ss

tst3 = runReader tstR2 ["a", "bb", "ccc"]


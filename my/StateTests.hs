import Control.Monad.Trans.Maybe
import           Control.Monad.State

fff :: MaybeT (State String) Char
fff = do
    modify (fmap succ)
    h <- gets head
    return h

-- > runState (runMaybeT (fff >>= return . succ)) "test"
-- (Just 'v',"uftu")
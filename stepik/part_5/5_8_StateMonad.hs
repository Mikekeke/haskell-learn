import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

modify1 :: (s -> s) -> State s ()
modify1 f = do 
    s <- get
    put $ f s

modify2 :: (s -> s) -> State s ()
modify2 f = get >>= put . f

-- valid task answer
-- readerToState :: Reader r a -> State r a
-- readerToState m = State $ \r -> ((runReader m r), r)

-- valid task answer
-- writerToState :: Monoid w => Writer w a -> State w a
-- writerToState m = 
    -- let
        -- (a, st') = runWriter m 
    -- in State $ \st -> (a, st' `mappend` st)

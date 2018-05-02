import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

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

tick :: State Int Int
tick = do
    n <- get
    put (n+1)
    return n

succ' :: Int -> Int
succ' x = execState tick x

--"sequence $ replicate a tick" производит эффект tick "a" раз
plus :: Int -> Int -> Int
plus a b = execState (sequence $ replicate a tick) b
t1 = runState (sequence $ replicate 5 tick) 6 -- == ([6,7,8,9,10],11)
t2 = runState (sequence_ $ replicate 5 tick) 6 -- == ((),11)
-- evalState :: State s a -> s -> a
-- execState :: State s a -> s -> s
-- runState :: State s a -> s -> (a, s)
t3 = evalState (sequence $ replicate 5 tick) 6 -- == [6,7,8,9,10]

plus' :: Int -> Int -> Int
plus' a b = execState (replicateM a tick) b

fibStep :: State (Integer, Integer) ()
fibStep = do
    (a,b) <- get
    put(b, a+b)
    -- return () -- don't need here, put alredy gives State

-- better form course stuff
fibStep' :: State (Integer, Integer) ()
fibStep' = modify $ \(a,b) -> (b, a + b)

execStateN :: Int -> State s a -> s -> s
execStateN n m = execState (replicateM_ n m)

fib :: Int -> Integer
fib n = fst $ execStateN n fibStep' (0, 1)


-- Number tree
data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving Show

-- some straight approach where I saw state "pattern"
nTree :: Tree() -> Integer -> (Tree Integer, Integer)
nTree (Leaf ()) n     = ((Leaf n), n+1)
nTree (Fork l () r) n = ((Fork l' x r'), n3) where
    (l', n2) = nTree l n
    x = n2+1
    (r', n3) = nTree r (n+2)

-- my variant
stepTree :: Tree () -> State Integer (Tree Integer)
stepTree (Leaf _) = do
        n <- get
        put (n+1)
        return $ Leaf n

stepTree (Fork l _ r) = do
        l' <- stepTree l
        x' <- get
        modify (+1)
        r' <- stepTree r
        return $ Fork l' x' r'

numberTree :: Tree () -> Tree Integer
numberTree tree = evalState (stepTree tree) 1

-- more elegat course stuff variant
numberTree' :: Tree () -> Tree Integer
numberTree' tree = evalState (number tree) 1
  where
    number :: Tree () -> State Integer (Tree Integer)
    number (Leaf ()) = get >>= \i -> modify (+1) >> return (Leaf i)
    number (Fork l () r) = do
      la <- number l
      i <- get
      modify (+1)
      ra <- number r
      return $ Fork la i ra

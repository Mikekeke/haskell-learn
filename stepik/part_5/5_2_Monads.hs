import           Control.Monad

data Log a = Log [String] a deriving Show

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f message = Log [message] . f

add1Log = toLogger (+1) "added one"
mult2Log = toLogger (* 2) "multiplied by 2"

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x l1 l2 = Log [msg1, msg2] r2 where
    Log [msg1] r1 = l1 x
    Log [msg2] r2 = l2 r1

toKleisli :: Monad m => (a -> b) -> a -> m b
toKleisli f x = return (f x)
-- toKleisli = (return .)

returnLog :: a -> Log a
returnLog = Log []

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log msgs v) f = Log (msgs++msg) result where
    Log msg result = f v


instance Functor Log where
    fmap = liftM

instance Monad Log where
    return = returnLog
    (>>=) = bindLog

instance Applicative Log where
    pure  = return
    (<*>) = ap

-- my. Passed test, but not coorect, will fail in case of "execLoggersList x []"
-- execLoggersList :: a -> [a -> Log a] -> Log a
-- execLoggersList a (f:fs) = foldl (>>=) (f a) fs

-- not my, good ones
execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList a = foldl (>>=) $ return a

-- execLoggersList :: a -> [a -> Log a] -> Log a
-- execLoggersList = foldl (>>=) . return

-- execLoggersList :: a -> [a -> Log a] -> Log a
-- execLoggersList x []       = return x
-- execLoggersList x (f : fs) = f x >>= (flip execLoggersList $ fs)

-- not my END

f1 x = Just $ x+1
f2 x = if x > 10 then Nothing else Just $ x+10
compos = f1 <=< f2
testJ14 = return 3 >>= compos
testNoth = return 11 >>= compos

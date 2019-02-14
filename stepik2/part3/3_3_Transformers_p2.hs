import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Debug.Trace

tickCollatz :: State Integer Integer
tickCollatz = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  put res
  return n

type EsSi = ExceptT String (State Integer)

runEsSi :: EsSi a -> Integer -> (Either String a, Integer)
runEsSi = runState . runExceptT

go :: Integer -> Integer -> State Integer Integer -> EsSi ()
go lowerBound upperBound st = do 
    lift (modify $ execState st)
    s <- lift get
    when (s <= lowerBound) (throwE "Lower bound")
    when (s >= upperBound) (throwE "Upper bound")

    
{-
forever     :: (Applicative f) => f a -> f b
forever a   = let a' = a *> a' in a'

-}

tst1 = runEsSi (go 1 85 tickCollatz) 27                           == (Right (),82)
tst2 = runEsSi (go 1 80 tickCollatz) 27                           == (Left "Upper bound",82)
tst3 = runEsSi ((forever $ go 1 1000 tickCollatz) :: EsSi ()) 27  == (Left "Upper bound",1186)
tst4 = runEsSi ((forever $ go 1 10000 tickCollatz) :: EsSi ()) 27 == (Left "Lower bound",1)
tsts = mapM_ (putStrLn . show) $ zip [1..] [tst1, tst2, tst3, tst4]

{-
!!! stuffs solution
!!! lift :: (MonadTrans t, Monad m) => m a -> t m a

go :: Integer -> Integer -> State Integer Integer -> EsSi ()
go lower upper next = do
  lift next -- Лифтить можно любое вычисление во внутренней монаде
  n <- lift get
  when (n <= lower) (throwE "Lower bound")
  when (n >= upper) (throwE "Upper bound")
-}
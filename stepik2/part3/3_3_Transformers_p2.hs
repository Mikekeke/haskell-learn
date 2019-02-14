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
  e <- lift get undone
  let x = evalState st (traceShowId e)
  when ((traceShowId x) <= lowerBound) (throwE "Lower bound")
  when (x >= upperBound) (throwE "Upper bound")

{-
forever     :: (Applicative f) => f a -> f b
forever a   = let a' = a *> a' in a'

GHCi> runEsSi (go 1 85 tickCollatz) 27
(Right (),82)
GHCi> runEsSi (go 1 80 tickCollatz) 27
(Left "Upper bound",82)
GHCi> runEsSi (forever $ go 1 1000 tickCollatz) 27
(Left "Upper bound",1186)
GHCi> runEsSi (forever $ go 1 10000 tickCollatz) 27
(Left "Lower bound",1)
-}
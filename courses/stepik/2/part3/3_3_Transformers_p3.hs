import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

type RiiEsSiT m = ReaderT (Integer,Integer) (ExceptT String (StateT Integer m))

tickCollatz' :: StateT Integer IO Integer
tickCollatz' = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  lift $ putStrLn $ show res
  put res
  return n

runRiiEsSiT :: ReaderT (Integer,Integer) (ExceptT String (StateT Integer m)) a 
                 -> (Integer,Integer) 
                 -> Integer 
                 -> m (Either String a, Integer)

runRiiEsSiT rt env st = runStateT (runExceptT (runReaderT rt env)) st 
-- runRiiEsSiT = ((runStateT . runExceptT) .) . runReaderT

go :: Monad m => StateT Integer m Integer -> RiiEsSiT m ()
go stT= do
    (lower, upper) <- ask
    lift $ lift stT
    n <- lift $ lift get
    when (n <= lower) (lift $ throwE "Lower bound")
    when (n >= upper) (lift $ throwE "Upper bound")

{-
from solutions
go :: Monad m => StateT Integer m Integer -> RiiEsSiT m ()
go action = do
  (low, high) <- MTL.ask
  void $ lift $ lift action
  current <- MTL.get
  when (current <= low)  $ MTL.throwError "Lower bound"
  when (current >= high) $ MTL.throwError "Upper bound"
-}


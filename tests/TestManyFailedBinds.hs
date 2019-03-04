import Control.Monad
import Debug.Trace

data Mb a = Nok | Ok a deriving Show

instance Functor Mb where fmap = liftM
instance Applicative Mb where {pure = return; (<*>) = ap}
instance Monad Mb where 
    return = Ok
    (Ok x) >>= k = trace "Ok bind" $ k x
    Nok >>= _ = trace "Nok bind" Nok

testFun :: Enum a => a -> Mb a
testFun = Ok . succ
kArrsCmps :: Enum a => a -> Mb a
kArrsCmps = foldr1 (>=>) (replicate 3 testFun)

{-
λ: Ok 0 >>= kArrsCmps
Ok bind
Ok bind
Ok bind
Ok 3
λ: Nok >>= kArrsCmps
Nok bind
Nok
-}

-- BUT if not composed:
testNotComposed = Nok >>= return >>= return
{-
λ: testNotComposed 
Nok bind
Nok bind
Nok    
-}
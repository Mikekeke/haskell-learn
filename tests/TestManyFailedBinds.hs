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

testFun2 ::Int -> Mb Int
testFun2 = \x -> if x < 2 then Ok (succ x) else Nok
kArrsCmps2 :: Int -> Mb Int
kArrsCmps2 = foldr1 (>=>) (replicate 10 testFun2)
{-
λ: Ok 0 >>= kArrsCmps2 
Ok bind
Ok bind
Ok bind
Nok bind
Nok
-}

tst3 = Ok 0 >>= testFun2 >>= testFun2 >>= testFun2 >>= testFun2 >>= testFun2 >>= testFun2 >>= testFun2
{-
λ: tst3
Ok bind
Ok bind
Ok bind
Nok bind
Nok bind
Nok bind
Nok bind
Nok
-}

kk = \_ -> trace "n" Nothing
tst4 = Just 1 >>= (kk >=> kk >=> kk)
tst5 = Just 1 >>= kk >>= kk >>= kk

kk2 = \_ -> trace "n" Nok
tst6 = Ok 1 >>= kk2 >>= kk2 >>= kk2

main = putStrLn $ show tst3
{-
main run when compiled with -O2
Ok bind
Ok bind
Ok bind
Nok bind
Nok

main run w/o -O2
Ok bind
Ok bind
Ok bind
Nok bind
Nok bind
Nok bind
Nok bind
Nok
-}
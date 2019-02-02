import Control.Monad.Cont
import Debug.Trace

ex1 :: Cont Int Int
ex1 = do
    a <- return 1
    b <- cont (\f -> (f a) + (f a))
    c <- return 3
    return $ traceShow (a,b,c) $ a + b + c



testl1 = [3,2,1,0,1,2,3,4]
testl2 = [1,2,3]
prod :: [Integer] -> Integer
prod [] = 0
prod (x:[]) = x
prod (x:xs) | x == 0 = 0
            | otherwise = trace "mult" $ x * (prod xs)

prodContStep :: Integer -> Cont Integer Integer
-- prodContStep 0 = cont (\_ -> 0) 
prodContStep 0 = cont (const 0) 
prodContStep x = return x

prodCnt [] = cont (\_ -> 0)
prodCnt (x:[]) = return x
prodCnt (x:xs) = do
    v1 <- prodContStep x
    rest <- prodCnt xs
    return $ trace "cont mult" $ v1 * rest

tstProdCont l = runCont (prodCnt l) id

prodCnt2 [] = cont (const 0)
-- prodCnt2 (0:_) = cont (const 0) 
-- prodCnt2 (x:[]) = return x
-- prodCnt2 (x:xs) = do
--     rest <- prodCnt xs
--     return $ trace "cont mult" $ x * rest
prodCnt2 (x:xs) | x == 0 = cont (const 0) 
                | null xs = return x
                | otherwise = prodCnt xs >>= return . trace "cont mult" (x*)

{-
Way to do efficient product with Cont:
"tstProdCont testl1" should not trace any "cont mult"
coz will pass "cont (\_ -> 0)" instead all preceeding computation when encounters 0
unlike "prod", which will still perform 3 multiplications till reach 0

how base does "product" can't understand yet
-}

prodFold [] = 0
prodCnt (x:xs) = foldr f 1 (x:xs) where
     
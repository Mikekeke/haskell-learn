import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.State

go _ [] = (Nothing, [])
go n (x:xs) | n == 0 = (Just x, xs)
            | otherwise = let (result, rest) = go (n-1) xs in (result, x:rest)

run n l = case go n l of 
    (Nothing, l') -> l'
    (Just x, rest) -> x : rest

square x c = c (x^2)

add a b c = c (a+b)

prod1 :: (Num a, Eq a) => [a] -> a
prod1 [] = 0
prod1 (0:_) = 0
prod1 (x:xs) = x * prod1 xs

prod2 :: (Num a, Eq a) => [a] -> ([a] -> a ) -> a
prod2 [] f = 0
prod2 (0:_) f = undefined
prod2 (x:xs) f = x * prod1 xs

reqNum :: Monad m => Int -> ExceptT String m Int
reqNum 1 = return 33
reqNum 2 = throwError "err num"

reqData :: Monad m => Int -> ExceptT String m String
reqData 2 = return "OK data"
reqData 1 = throwError "BAD data"

data Empty
data Created
data Downloaded

data ActS a = ActState {num :: Maybe Int, dt :: Maybe String} deriving Show

defActS :: ActS Empty
defActS = ActState Nothing Nothing

setNum :: Int -> ActS Empty -> ActS Created
setNum n as = as {num = return n}
setDt s as = as {dt = return s}

-- tst1 :: Either ActS ActS
tst1 :: Int -> ExceptT String (State (ActS a)) ()
tst1 rn = do
    num <- reqNum rn
    modify (setNum num)
    dat <- reqData rn
    modify (setDt dat)
    
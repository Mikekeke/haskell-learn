import           GHC.Arr         as A

fibExp 0 = 0
fibExp 1 = 1
fibExp n = fibExp (n-1) + fibExp (n-2)


fibMem 0 = 0
fibMem 1 = 1
fibMem n =  memo ! (n-2) + memo ! (n-1) where
    memo = A.listArray (0,n) (fibMem <$> [0..]) -- setting up memo array on every call, see fixed below

fibMemFixed n = go n where
    go 0 = 0
    go 1 = 1
    go n = memo ! (n-2) + memo ! (n-1)
    memo = A.listArray (0,n) (go <$> [0..])

fibMemFixed2 n = fmap go (A.listArray (0,n) ([0..])) ! n where -- does not work like memoized_fib
    go 0 = 0
    go 1 = 1
    go n = fibMemFixed2 (n-2) + fibMemFixed2 (n-1)

--https://wiki.haskell.org/Memoization
memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = memoized_fib (n-2) + memoized_fib (n-1)
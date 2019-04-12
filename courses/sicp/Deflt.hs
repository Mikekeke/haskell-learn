{-# LANGUAGE BangPatterns #-}


fn1 n | n < 3 = n
      | otherwise = 
        let 
            r1 = fn1 (n-1)
            r2 = 2 * fn1 (n-2)
            r3 = 3 * fn1 (n-3)
        in r1 + r2 + r3

{-
to understand why 2 1 0
We can also formulate an iterative process for computing the Fibonacci numbers. 
The idea is to use a pair of integers a and b, initialized to Fib(1) = 1 and Fib(0) = 0, 
and to repeatedly apply the simultaneous transformations
a←a+b,
b←a.
It is not hard to show that, after applying this transformation n times,
 a and b will be equal, respectively, to Fib(n+1) and Fib(n)
-}
fn n = if n < 3 then n else fn' 2 1 0 n where
    fn' a b c n' | n' < 3 = a
                 | otherwise = let a' = (a + 2*b + 3*c)
                                   b' = a 
                                   c' = b
                                in fn' a' b' c' (pred n')

{-from book fib-}
-- exponent growth
fibExp 0 = 0
fibExp 1 = 1
fibExp n = fibExp (n-1) + fibExp (n-2)

-- ~const memory
fibConst n = go 1 0 n where
    go a b 0 = b
    go a b n' = go (a+b) a (pred n')




fn2 sum' n | n < 3 = sum'
           | otherwise = fn2 (n-1) sum'
{-
fn2 10 4 == 8
fn2 10 4
fn2 (4-1) 10
fn2 3 10
fn2 (10 - 1) 3
fn2 9 3
fn2 (3 - 1) 9
fn2 2 9
fn2 (9 - 1) 2
fn2 8 2
8


-}

fn2' sum' n cnt | n < 3 = (sum', cnt)
                | otherwise = fn2' (n-1) sum' (succ cnt)
{-
λ: fn2' 10 4 0
(8,4)
4 iterations for 2 substractions
-}


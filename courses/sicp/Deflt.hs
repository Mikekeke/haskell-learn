{-# LANGUAGE BangPatterns #-}
fn1 n | n < 3 = n
      | otherwise = 
        let 
            r1 = fn1 (n-1)
            r2 = 2 * fn1 (n-2)
            r3 = 3 * fn1 (n-3)
        in r1 + r2 + r3

fn3 n = if n < 3 then (n,0) else fn3' 2 1 0 n 0 where
    fn3' a b c n' iteration | n' < 3 = (a,iteration)
                            | otherwise = fn3' (a + 2*b + 3*c) a b (pred n') (succ iteration)


fn1_2 n | n < 3 = n
      | otherwise = 
        let 
            r1 = fn1_2 (n-1)
            r2 = 2 * fn1_2 (n-2)
            r3 = 3 * fn1_2 (n-3)
            sm !a !b !c = a + b + c
        in sm r1 r2 r3


{-
!!! in prelude, not compiled
λ: fn3 20
(10771211,18)
(0.01 secs, 73,528 bytes)
λ: fn1 20
10771211
(0.24 secs, 56,763,920 bytes)
for compiled see stats1
-}

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

-- main = putStrLn (show $ fn1 30)
-- main = putStrLn (show $ fn3 30)
main = putStrLn (show $ fn1_2 30)
-- 2.2
fib :: Int -> Int
fib = go 0 1 where
    go _ x 1 = x
    go a b n = go b (a+b) (pred n)


run f = getLine >>= putStrLn . show . f .read
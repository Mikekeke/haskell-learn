fibStream :: [Integer]
fibStream = 0 : 1 : zipWith (+) (fibStream) (tail fibStream)


repeat' :: a -> [a]
repeat' x = xs where xs = x : xs

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate f (f x)

repeatHelper = id
repeat'' = iterate repeatHelper
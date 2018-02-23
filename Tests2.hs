class Container a where
    put ::  b -> a b
    get :: a b -> b

instance Container [] where
    put a = [a]
    get (a:[]) = a
    get _ = error "just error, man"

instance Container Maybe where
    put a = Just a
    get (Just a) = a
    get _ = error "can't get from nothing"

listPut:: a -> [a]
listPut = put

-- Artyom, [15.02.18 23:48]
-- 1. reverse (put 3) – сработает, потому что выведется, что там должен быть список (прим. :t reverse - это [a] -> [a])
--
-- 2. put 3 :: [Int]
--
-- 3. put @[] 3 (с включенным TypeApplications – http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#ghc-flag--XTypeApplications)
--
-- Pig Greenest, [15.02.18 23:50]
-- Ещё вариант asTypeOf (put 3) []
--
-- Pig Greenest, [15.02.18 23:51]
-- https://hackage.haskell.org/package/base-4.10.1.0/docs/Prelude.html#v:asTypeOf
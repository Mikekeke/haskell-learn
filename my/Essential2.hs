test1 = [Just 1, Just 2, Just 3]
test2 = [Just 1, Nothing, Just 3]

sequence' :: Monad m => [m a] -> m [a]
sequence' []     = return []
sequence' (x:xs) = x >>= \v -> fmap ((:) v) (sequence' xs)
-- Just 4 >>= \4 -> fmap (4:) (Just [])


sequence'' :: Monad m => [m a] -> m [a]
sequence'' ms = foldr k (return []) ms where
    k a z = do
        x <- a
        xs <- z
        return $ x:xs



data Lol = Lol Int
bnd c = Just Lol >>= \x -> return $ c x
bnd' c = Just Lol >>= return .($ c)



-- *****************
fromStr :: Monad m => String -> m Int
fromStr s = if length s == 2 then return $ read s else fail "myErr"
-- fromStr "12" :: Maybe Int ~> Just 12
-- fromStr "1" :: Maybe Int ~> Nothing
-- fromStr "1" :: Either String Int ~> *** Exception: myErr

process1 :: Int -> [Int]
process1 = return . (+2)

process2 :: Int -> Maybe Int
process2 = return . (+2)

-- if fromStr fails, any tries to call isRigt or isLeft will end with *** Exception: err
process3 :: Int -> Either String Int
process3 x = Right x
testEth:: Either a b -> String -- will fall with error if fromStr failed
testEth (Right _) = "right"
testEth (Left _)  = "left"
process31 :: Int -> Either String Int
process31 x = if x > 12 then Right x else Left "wops"


calc :: Monad m => String -> (Int -> m Int) -> m Int
calc s process = do
    x' <- fromStr s
    x'' <- process x'
    return x''

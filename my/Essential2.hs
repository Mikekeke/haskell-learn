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

action :: String -> ExceptT String IO ()
action = liftIO . putStrLn

test1 = let ac1 = traverse action (Just "test") in runExceptT ac1
test2 = let ac1 = traverse action Nothing in runExceptT ac1

{-
λ: test1
test
Right (Just ())
λ: test2
Right Nothing
-}
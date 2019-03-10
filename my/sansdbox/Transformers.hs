import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.State

-- loosing log
tst1 :: WriterT String (Except String) String -- Adding writing inside excpet
tst1 = do 
    tell "one;"
    tell "two;"
    throwError "err"

tst1Run :: Either String (String, String)
tst1Run = runExcept . runWriterT $ tst1 -- Left "err"

-- NOT loosing log
tst2 :: ExceptT String (Writer String) String -- Adding error handling inside writing
tst2 = do 
    tell "one;"
    tell "two;"
    throwError "err"

tst2Run :: (Either String String, String)
tst2Run = runWriter . runExceptT $ tst2 -- (Left "err","one;two;")

-- save state with error
type StateExcpet e s a = ExceptT e (State s) a

test3 :: Int -> StateExcpet String String ()
test3 limit =  do
    modify (succ . head >>= (:)) -- takes first char from state and adds next one in alphabet to state
    s <- get
    when (length s == limit) (throwError $ "State reached limit of " ++ show limit)

runTest3 :: ExceptT String (State String) () -> (Either String (), [Char])
runTest3 se = runState (runExceptT se) "a"

{-
λ: runTest (forever $ test 4)
(Left "State reached limit of 4","dcba")
λ: runTest (replicateM_ 2 $ test 4)
(Right (),"cba")
-}

test4 :: Monad m => Bool -> ExceptT String (StateT String m) Int
test4 shouldFail = do
    modify (join . replicate 2)
    when shouldFail (throwError "failed")
    return 1

runTest4 :: Monad m => Bool -> m (Either String Int, String)
runTest4 sf = runStateT (runExceptT $ test4 sf) "abc"

runTest4_1 ::  Maybe (Either String Int, String)
runTest4_1 = runTest4 True -- Just (Left "failed","abcabc")
runTest4_2 ::  Maybe (Either String Int, String)
runTest4_2 = runTest4 False -- Just (Right 1,"abcabc")
runTest4_3 ::  Either String (Either String Int, String)
runTest4_3 = runTest4 True -- Right (Left "failed","abcabc")
runTest4_4 ::  Either String (Either String Int, String)
runTest4_4 = runTest4 False -- Right (Right 1,"abcabc")


test4' :: Monad m => Bool -> ExceptT String (StateT String m) Int
test4' shouldFail = do
    modify (join . replicate 2)
    when shouldFail (throwError "failed")
    fail "Base monad fail"
    return 1

runTest4' :: Monad m => Bool -> m (Either String Int, String)
runTest4' sf = runStateT (runExceptT $ test4' sf) "abc"
runTest4_2' ::  Maybe (Either String Int, String)
runTest4_2' = runTest4' False -- Nothing
runTest4_4' ::  Either String (Either String Int, String)
runTest4_4' = runTest4' False -- *** Exception: Base monad fail

-- ************************************************

t5f2 :: Int -> Except String Int
t5f2 x = throwError $ "test5: " ++ show x

fromEx :: Monad m => Except e a -> ExceptT e m a
fromEx = ExceptT . return . runExcept

test5 :: Monad m => ExceptT String m Int
test5 = do
    fromEx $ t5f2 666
    return 1
{-
λ: runExceptT test5 :: Maybe (Either String Int)
Just (Left "test5: 666")
-}


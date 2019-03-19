{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.Monoid
import Control.Monad.Except (runExcept)
import Data.Foldable

data ReadError = EmptyInput | NoParse String
  deriving Show
  
tryRead :: (Read a, MonadError ReadError m) => String -> m a
tryRead [] = throwError EmptyInput
tryRead s = case reads s of
  (x, []): _ -> return x
  _         -> throwError $ NoParse s


{-***********************************************-}
data Tree a = Leaf a | Fork (Tree a) a (Tree a)


tryReadWithLog a = tryRead a >>= tell . Sum

treeSum :: Tree String -> Either ReadError Integer
treeSum t = (getSum . snd) <$> runWriterT (go t) 
      where
        go :: MonadError ReadError m => Tree String -> WriterT (Sum Integer) m ()
        go (Leaf a) = tryReadWithLog a
        go (Fork l a r) = go l >> tryReadWithLog a >> go r

{-
GHCi> treeSum $ Fork (Fork (Leaf "1") "2" (Leaf "oops")) "15" (Leaf "16")
Left (NoParse "oops")
GHCi> treeSum $ Fork (Fork (Leaf "1") "2" (Leaf "0")) "15" (Leaf "16")
Right 34
-}

{-
from solutions (there was Traversable for tree, meh)
-- no mtl
import Control.Monad.Except (runExcept)
import Data.Foldable (sum)

treeSum :: Tree String -> Either ReadError Integer
treeSum = fmap sum . runExcept . traverse tryRead

-- mtl
treeSum :: Tree String -> Either ReadError Integer
treeSum = (getSum <$>) . execWriterT . traverse ((>>= tell . Sum).tryRead)

-}

{-****************************************************************-}
limited p fs = traverse limit1 (zip [0..] fs)
  where
    limit1 (i, f) = do
      a <- state (runState f)
      stateIsBad <- gets (not . p)
      when stateIsBad $ throwError i
      pure a

runLimited1 :: (s -> Bool) -> [State s a] -> s -> (Either Int [a], s)
runLimited1 p fs s = run1 (limited p fs) s

runLimited2 :: (s -> Bool) -> [State s a] -> s -> Either Int ([a], s)
runLimited2 p fs s = run2 (limited p fs) s

run1 = runState . runExceptT

-- run2 m s = runExcept (runStateT m s)
run2 = (runExcept .) . runStateT
-- run2 = runStateT -- can use just this, coz there is MonadError for Either and types will do the trick

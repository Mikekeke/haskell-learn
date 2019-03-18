{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Except

data ReadError = EmptyInput | NoParse String
  deriving Show
  
tryRead :: (Read a, MonadError ReadError m) => String -> m a
tryRead [] = throwError EmptyInput
tryRead s = case reads s of
  (x, []): _ -> return x
  _         -> throwError $ NoParse s
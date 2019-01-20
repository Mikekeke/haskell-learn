newtype Except e a = Except {runExcept :: Either e a }

withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept f ex = Except $ case runExcept ex of
    Left e -> Left . f $ e
    Right x -> Right x

{-
from answers
import           Control.Arrow
withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept  f =  Except . left f . runExcept
--
import Data.Bifunctor
withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept f (Except either) = (Except (first f either))
--
withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept f ex = except $ either (Left . f) (Right . id) (runExcept ex)
--
import Data.Bifunctor
withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept f = except . bimap f id . runExcept
-}
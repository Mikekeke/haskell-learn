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

data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex 
  deriving (Eq, Show)

(!!!) :: [a] -> Int -> Except ListIndexError a
xs !!! n
  | n < 0     = Except $ Left ErrNegativeIndex
  | otherwise = resultFun n
                    where 
                        resultFun = foldr f (Except . Left . ErrIndexTooLarge . const n) xs
                        f = (\x r i -> case i of
                            0 -> Except . Right $ x
                            _ -> r(i-1))
{-
-- best -- null работает на бесконечных списках
null :: t a -> Bool
null = foldr (\_ _ -> False) True

xs !!! n
  | n < 0 = throwE ErrNegativeIndex
  | null $ drop n xs = throwE $ ErrIndexTooLarge n
  | otherwise = return $ xs !! n

-- stuff --
(!!!) _ i | i < 0 = throwE $ ErrNegativeIndex
(!!!) xs n = go xs n 
  where
    go [] _ = throwE $ ErrIndexTooLarge n
    go (x : xs) 0 = pure x
    go (x : xs) i = go xs (i - 1)
-}

test1 l =  f2 l
    where 
        f1 :: a -> (a -> a) -> a -> a
        f1 = undefined
        f2 :: (Foldable t, Num a) => t a -> a -> a
        -- f2 :: t a -> (a -> a)
        f2 = foldr f1 (+10)

ft1 a b c = undefined
fff = foldr ft1 id "Asd"
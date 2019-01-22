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
                            _ -> r (i-1))
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


-- to clarify !!
-- http://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.List.html#%21%21

f' x f y = if y == 1 then f x else f (y - 1)
fff = foldr (\x f y -> if y == 1 then f x else f (y - 1)) show [10..11]
--    foldr (\x f -> \y -> ...)
{-
fn = (\x f y -> if y == 1 then f x else f (y - 1))
foldr fn show [10,11]
fn 10 (foldr fn show [11])
fn 10 (fn 11 (foldr fn [] show))
fn 10 (fn 11 (show))
fn 10 (fn 11 show)
fn 10 (\11 show -> \y -> if y == 1 then ...)
fn 10 (\y -> if y == 1 then show 11 else show (y - 1))
\10 (\y -> if y == 1 then show 11 else show (y - 1)) -> y1 -> if y1 == 1 then ...
-- show swapped to this ^^^ lambda
y1 -> if y1 == 1 then (\y -> if y == 1 then show 11 else show (y - 1)) 10 else (\y -> if y == 1 then show 11 else show (y - 1)) (y - 1))
y1 = 1
~> (\y -> if y == 1 then show 11 else show (y - 1)) 10
~> if 10 == 1 then show 11 else show (10 - 1))
~> 9
-}

f'' x f y = if y == 1 then f x else f (y - 1)
fff2 = foldr (\x f y -> if y == 1 then x else f (y - 1)) id [10..11]
{-
fn = \x f y -> if y == 1 then x else f (y - 1)
fn 10 (fn 11 id)
fn 10 (\y -> if y == 1 then 11 else id (y - 1))
\y1 -> if y == 1 then 10 else f (\y -> if y == 1 then 11 else id (y - 1)) (y1 - 1)

y1 = 1
if 1 == 1 then 10 else (\y -> if y == 1 then 11 else id (y - 1)) (y1 - 1)
10

y1 = 2
if 2 == 1 then 10 else (\y -> if y == 1 then 11 else id (y - 1)) (2 - 1)
(\y -> if y == 1 then 11 else id (y - 1)) 1
if 1 == 1 then 11 else id (1 - 1))
11

y1 = 4
if 4 == 1 then 10 else (\y -> if y == 1 then 11 else id (y - 1)) (4 - 1)
(\y -> if y == 1 then 11 else id (y - 1)) 3
if 3 == 1 then 11 else id (3 - 1))
id 2
2

-}
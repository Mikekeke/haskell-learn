{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import Control.Monad.Trans.Except
import Data.Semigroup
import Data.Monoid
import Data.Foldable (msum)
import Control.Monad
import Control.Applicative


data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex 
  deriving (Eq, Show)

(!!!) :: [a] -> Int -> Except ListIndexError a
xs !!! n
  | n < 0     = throwE ErrNegativeIndex
  | otherwise = resultFun n
                    where 
                        resultFun = foldr f (throwE . ErrIndexTooLarge . const n) xs
                        f = (\x r i -> case i of
                            0 -> return x
                            _ -> r (i-1))

newtype SimpleError = Simple { getSimple :: String } 
  deriving (Eq, Show)

instance Semigroup SimpleError where
  (<>) = mappend

instance Monoid SimpleError where
  mempty = Simple ""
  Simple a `mappend` Simple b = Simple $ a ++ b

lie2se :: ListIndexError -> SimpleError
lie2se (ErrIndexTooLarge i) = Simple $ "[index (" ++ show i ++ ") is too large]"
lie2se ErrNegativeIndex = Simple "[negative index]"

toSimple = runExcept . withExcept lie2se
xs = [1,2,3]
toSimpleFromList = runExcept . msum . map (withExcept lie2se)
{-
GHCi> toSimple = runExcept . withExcept lie2se
GHCi> xs = [1,2,3]
GHCi> toSimple $ xs !!! 42
Left (Simple {getSimple = "[index (42) is too large]"})
GHCi> toSimple $ xs !!! (-2)
Left (Simple {getSimple = "[negative index]"})
GHCi> toSimple $ xs !!! 2
Right 3
GHCi> import Data.Foldable (msum)
GHCi> toSimpleFromList = runExcept . msum . map (withExcept lie2se)
GHCi> toSimpleFromList [xs !!! (-2), xs !!! 42]
Left (Simple {getSimple = "[negative index][index (42) is too large]"})
GHCi> toSimpleFromList [xs !!! (-2), xs !!! 2]
Right 3
-}

{- from solutions
ShowS pattern
https://stackoverflow.com/questions/9197913/what-is-the-shows-trick-in-haskell
https://wiki.haskell.org/Difference_list

lie2se :: ListIndexError -> SimpleError
lie2se x = Simple ('[': display x "]") where
  display ErrNegativeIndex = mappend "negative index"
  display (ErrIndexTooLarge i) = mappend "index (" .
                                 shows i .
                                 mappend ") is too large"

-}

data ReadError = EmptyInput | NoParse String deriving Show
data SumError = SumError Int ReadError deriving Show

tryRead :: Read a => String -> Except ReadError a
tryRead []                   = throwE EmptyInput
tryRead s = case reads s of
  (x, []): _ -> return x
  _         -> throwE $ NoParse s

trySum :: [String] -> Except SumError Integer
trySum [] = return 0
trySum s = (fmap sum) . mapM readWithIdx . zip [1..] $ s where
    readWithIdx (i,v) = withExcept (SumError i) (tryRead v)

newtype Validate e a = Validate { getValidate :: Either [e] a } deriving Show

collectE :: Except e a -> Validate e a
collectE = Validate . either (Left . pure) Right . runExcept

instance Functor (Validate e) where
    fmap f = Validate . fmap f . getValidate

instance Applicative (Validate e) where
    pure = Validate . Right
    (<*>) (Validate (Right f)) = fmap f
    (<*>) (Validate (Left e)) = Validate . either (Left . mappend e) (const $ Left e) . getValidate

instance Monad (Validate e) where
    return = pure
    (Validate v) >>= k = either (Validate . Left) k  v

validateSum :: [String] -> Validate SumError Integer
validateSum s = go 1 s where
        readWithIdx i v = collectE (withExcept (SumError i) (tryRead v))
        go _ [] = pure 0
        go n (s':ss') = (+) <$> readWithIdx n s' <*> go (succ n) ss'

-- after seing solutions
-- can just traversem coz Validate has Applicative, but mind stuck on monad context and forgot traverse 
validateSum' = (fmap sum) . traverse readWithIdx . zip [1..] where
    readWithIdx (i, v) = collectE (withExcept (SumError i) (tryRead v))

-- traverse
-- instance Traversable [] where
--     traverse f = foldr (\a b -> liftA2 (:) (f a) b) (pure [])
--     traverse f = foldr (\a b -> (:) <$> f a <*> b) (pure [])


-- (:) <$> Validate (Right 20) <*> ((:) <$> Validate (Right 30) <*>  Validate (Right []))
-- (:) <$> Validate (Right 20) <*> (Validate (Right (30:) <*>  Validate (Right []))
-- (:) <$> Validate (Right 20) <*> (Validate (Right (30:[]))
-- Validate (Right (20:)) <*> (Validate (Right (30:[]))
-- Validate (Right (20:30:[]))

{-
traverse readWithIdx . zip [1..] $ ["10", "20"] :: Validate SumError [Int]
Validate {getValidate = Right [10,20]}
-}

{-
Sum monoid solution

import Data.Monoid (Sum(..))

instance Monoid a => Monoid (Validate e a) where
  mempty = Validate $ Right mempty
  (Validate (Left e1))  `mappend` (Validate (Left e2))  = Validate (Left $ e1 ++ e2)
  (Validate (Left e1))  `mappend` (Validate _)          = Validate $ Left e1
  (Validate _)          `mappend` (Validate (Left e2))  = Validate $ Left e2
  (Validate (Right v1)) `mappend` (Validate (Right v2)) = Validate $ Right (v1 `mappend` v2)


instance Functor (Validate e) where
  fmap f (Validate x) = Validate (f <$> x)


validateSum :: [String] -> Validate SumError Integer
validateSum =
  fmap getSum .
  mconcat .
  map (\(i, s) -> Sum <$> (collectE $ withExcept (SumError i) (tryRead s))) .
  zip [1..]


collectE :: Except e a -> Validate e a
collectE ex = Validate $ case (runExcept ex) of
  Left err -> Left [err]
  Right ok -> Right ok
-}

liftA21 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA21 f x = (<*>) (fmap f x)

liftA23 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA23 f a b = f <$> a <*> b


{-# LANGUAGE PatternSynonyms #-}

module NonZero
  ( NonZero()
  , pattern NonZero
  , unNonZero
  , nonZero
  ) where

newtype NonZero a = UnsafeNonZero a

pattern NonZero a <- UnsafeNonZero a

unNonZero :: NonZero a -> a
unNonZero (UnsafeNonZero a) = a

nonZero :: (Num a, Eq a) => a -> Maybe (NonZero a)
nonZero 0 = Nothing
nonZero i = Just (UnsafeNonZero i)

safeDivide :: Int -> NonZero Int -> Int
safeDivide i (NonZero j) = i `div` j
t1 = map (safeDivide 3 <$>) [nonZero 3, nonZero 0]

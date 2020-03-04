miffy :: Monad m => m Bool -> m x -> m x -> m x
miffy mb mt mf = do
  b <- mb
  if b then mt else mf

iffy :: Applicative a => a Bool -> a x -> a x -> a x
iffy ab at af = pure cond <*> ab <*> at <*> af where
  cond b t f = if b then t else f

a1 = print "A" >> return ()
a2 = print "B" >> return ()
test1 b = miffy (return b) a1 a2
test2 b = iffy (pure b) a1 a2

{-
λ: test1 False
"B"
λ: test2 False
"A"
"B"
λ: 
-}

a3 = Just "A"
a4 = Nothing
test3 b = miffy (return b) a3 a4
test4 b = iffy (pure b) a3 a4
{-
λ: test3 False
Nothing
λ: test3 True
Just "A"
λ: test4 False
Nothing
λ: test4 True
Nothing
-}
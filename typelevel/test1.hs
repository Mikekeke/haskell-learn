{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

data Proxy a = Proxy

data A = A String
data B = B String Int
data EOL

class Named a where
  name :: Proxy a -> String

instance Named A where name _ = "A"
instance Named B  where name _ = "B"
instance Named EOL where name _ = ""

instance (Named head, Named tail) => Named (head, tail) where
  name _ = name (Proxy :: Proxy head) ++ ", " ++ name (Proxy :: Proxy tail)

{-
λ: name (Proxy :: Proxy (A,(B,EOL)))
"A, B, "
λ: name (Proxy :: Proxy (B,(A,EOL)))
"B, A, "
-}
----------------------------------------------------------------------------------------------------

data A2 = A2 String
data B2 = B2 String Int
data EOL2

class Named2 a where
  name2 :: String

instance Named2 A2 where name2 = "A2"
instance Named2 B2  where name2 = "B2"
instance Named2 EOL2 where name2 = "EOL2"

instance (Named2 head, Named2 tail) => Named2 (head, tail) where
  name2 = name2 @head ++ ", " ++ name2 @tail

{-
λ: :set -XTypeApplications
λ: name2 @(A2,(B2,(A2,EOL2)))
"A2, B2, A2, EOL2"
-}

-- TODO examine further:
-- https://www.youtube.com/watch?v=0wxGrf8toWk
-- https://gist.github.com/aaronlevin/d3911ba50d8f5253c85d2c726c63947b
-- but rememeber, it's from 2016
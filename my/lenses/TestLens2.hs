import Data.Functor.Identity
import Data.Functor.Const
import Data.Function

over :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
over lens f = runIdentity . lens (Identity . f)

-- kind a my variant, works too lol
viewMy :: ((a -> Const a b) -> s -> Const a d) -> s -> a
viewMy lens = getConst . lens Const

-- from https://github.com/ekmett/lens/wiki/Derivation#folds
foldMapOf :: ((a -> Const r a) -> s -> Const r s) ->  (a -> r) -> s -> r
foldMapOf l f = getConst . l (Const . f)
-- kind a my variant from foldMapOf, kek, need to read all article
view l = foldMapOf l id

set lens v = over lens (const v) 
-- pfree: set lens = over lens . const 
-- more pfree: set = (. const) . over
third k (x:y:z:vs) = (\v' -> x:y:v':vs) <$> (k z)
third _ _ = error "Pattern match fail"
-- setThird k (x:y:z:vs) = Just $ (\v' -> x:y:v':vs) <$> (k z)
-- setThird _ _ = Nothing

l1 = "1234"
l2 = ["1234","5678", "asdf"]

setThrdLens = third `set` '0'
{-
λ: setThrdLens l1
"1204"
-}

{-
λ: (third . third) `set` '0' $ l2
["1234","5678","as0f"]

λ: (third . third) `set` '0' $ ["1234","5678"]
*** Exception: Pattern match fail
-}

-- "&" is handy, otherwise need "$" and not that readable: third `set` '0' $ l1
t1 = l2 & view third -- "asdf"
t2 = l1 & third `set` '0' -- "1204"

t3 = third `view` "1234" -- '3'
t4 = third `viewMy` "1234" -- '3'
t5 = third `view` l2 -- 'd'

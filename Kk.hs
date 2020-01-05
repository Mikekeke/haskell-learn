{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE KindSignatures            #-}

import Data.Functor.Const
import Data.Functor.Identity

data Record = Record {_uid :: Int, _data :: String} deriving Show
data Holder = Holder {rec :: Record}

{-
Record -> String
String -> Record -> Record
-}

preDataLns (Record uid _) = \v -> Record uid v

set    val record  = const val (preDataLns record)
modify f   record = f (preDataLns record)

get1 lns record = getConst (fmap (preDataLns record) (Const $ _data record))
set1 lns record = runIdentity (fmap (preDataLns record) (Identity $ _data record))

get2 field lns record = getConst    (fmap (lns record) (Const    $ field record))
set2 field lns record = runIdentity (fmap (lns record) (Identity $ field record))

-- both field lns record = (fmap (lns record) (functor $ field record))

preDataLns2 (Record uid v) fn f = (\v' -> Record uid (f v')) <$> (fn v)

both lns record = lns record

get3 lns record = getConst (both lns record Const id) 
mod3 lns f record = runIdentity (both lns record Identity f)
set3 lns v record = runIdentity (both lns record Identity (const v))


preDataLns4 (Record uid v) fn = (\v' -> Record uid v') <$> (fn v)

view lns a = getConst (lns a Const)
over lns a f = runIdentity (lns a (Identity . f))
mod4 = over
set4 lns a v = over lns a (const v)

-- preDataLns5 :: Functor f => (String -> f String) -> Record -> f Record
preDataLns5 fn (Record uid v) = (\v' -> Record uid v') <$> (fn v)
-- holderL5 :: forall (f :: * -> *). Functor f => (Record -> f Record) -> Holder -> f Holder
holderL5    fn (Holder rec)   = (\v' -> Holder v')     <$> (fn rec)

view5 lns a = getConst (lns Const a )
over5 lns f = runIdentity . lns (Identity . f)
mod5 = over5
set5 lns v = over5 lns (const v)

tst = let 
        -- f = holderL5 . preDataLns5
        h = Holder (Record 10 "test")
      in view5 (holderL5 . preDataLns5) h
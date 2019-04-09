{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

data C
data F

newtype Temp a = Temp Double deriving (Show, Num)

addTemp :: Temp a -> Temp a -> Temp a
addTemp (Temp x) (Temp y) = Temp $ x + y

tc :: Temp C
tc = 23
tf :: Temp F
tf = 10

r1 = addTemp tc tc 
r2 = addTemp tf tf
-- r3 = addTemp tc tf --wont compile

data TempScale = Cels | Fahr

data Temperature :: TempScale -> * where
    Fahrs :: Double -> Temperature 'Fahr
    Celss :: Double -> Temperature 'Cels

-- for Show see GadtDeriving.hs
instance Show (Temperature a) where
    show (Celss a) = show a ++ " C"
    show (Fahrs a) = show a ++ " F"


addTemp' :: Temperature d -> Temperature d -> Temperature d
addTemp' (Fahrs a) (Fahrs b) = Fahrs $ a + b
addTemp' (Celss a) (Celss b) = Celss $ a + b

tc' = Fahrs 23
tf' = Celss 10

r1' = addTemp' tc' tc' 
r2' = addTemp' tf' tf'
-- r3' = addTemp' tf' tc' --wont compile

data Temp'' = Fahrs'' Double | Celss'' Double deriving Show
addTemp'' (Fahrs'' a) (Fahrs'' b) = Fahrs'' $ a + b
addTemp'' (Celss'' a) (Celss'' b) = Celss'' $ a + b

r0 = addTemp'' (Fahrs'' 10) (Celss'' 20) -- compile, patmat runtime error